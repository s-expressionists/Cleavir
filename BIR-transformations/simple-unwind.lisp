(in-package #:cleavir-bir-transformations)

;;; In some cases, the dynamic environment of a nonlocal unwind
;;; can be understood statically. For example, in
;;; (block nil (mapcar (lambda (x) (return x)) list)),
;;; the lambda will be called in the same dynamic environment as
;;; the mapcar is called in, assuming a reasonable implementation
;;; of mapcar, and so the return/unwind dynamic environment can
;;; be understood by the compiler to have no unwind-protects or
;;; etc. to worry about. This can power optimizations.

;;; For example, the unwind need not run a dynamic search to ensure
;;; the exit point is still valid, or to find unwind-protect cleanups
;;; to execute. On a low level, they can be more or less a frame
;;; pointer reset followed by a jump.

;;; We call such unwinds "simple".

;;; If all unwinds to a given come-from are simple, additional
;;; optimizations are possible. The come-from instruction does not need
;;; to augment the dynamic environment, as it's only unwound to by
;;; unwinds that don't use the dynamic environment anyway.
;;; (Though indicating its existence to debugging tools may be prudent.)

;;; This code determines whether an unwind can be seen to be
;;; called in the dynamic environment output by their destination.
;;; It can also be used on come-from instructions, in which case it checks
;;; whether it's true of all the unwinds to it.

;;; This pass can and probably should be run after inlining.

;;; TODO: Much extension is possible:

;;; In cases where the unwind's dynamic environment has been
;;; augmented by the function that's unwinding, this pass does not
;;; understand the unwind as simple. Those unwinds could be made simple by
;;; preceding them with an appropriate local-unwind.
;;; This insertion can and probably should be done back in AST-to-BIR,
;;; not here, since it's valid regardless.

;;; Cases where the call's dynamic environment has been augmented
;;; from the come-from may also be possible to handle, but this might
;;; require some changes in the BIR representation of nonlocal exits.

;;; ALSO, might want to cache this information?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Used to treat recursive functions correctly.
(defvar *seen*)

;;; The below functions generally take a thing, and a destination dynamic
;;; environment, and determine if the thing is simple with respect to that
;;; destination.

(defgeneric simple-dynenv-p (client dynenv dest)
  (:method (client
            (dynenv bir:dynamic-environment)
            (dest bir:dynamic-environment))
    (declare (ignore client))
    (eq dynenv dest)))

(defmethod simple-dynenv-p (client
                            (dynenv bir:function)
                            (dest bir:dynamic-environment))
  (function-called-simply-p client dynenv dest))

(defgeneric simple-user-p (client user dest datum)
  (:method (client
            (user bir:instruction)
            (dest bir:dynamic-environment)
            datum)
    (declare (ignore client datum))
    nil))

(defmethod simple-user-p (client
                          (user bir:abstract-call)
                          (dest bir:dynamic-environment)
                          datum)
  (and (or (eq (bir:callee user) datum)
           (attributes:has-flag-p (bir:attributes user) :dyn-call))
       (simple-instruction-p client user dest)))

(defmethod simple-user-p (client
                          (user bir:abstract-local-call)
                          (dest bir:dynamic-environment)
                          (datum bir:function))
  (and (eq (bir:callee user) datum)
       (simple-instruction-p client user dest)))

(defun function-called-simply-p (client function come-from)
  ;; If we've already analyzing this function, we must have hit a recursive
  ;; call. In that case, we return true so the prior analysis can continue.
  (when (set:presentp function *seen*)
    (return-from function-called-simply-p t))
  (set:nadjoinf *seen* function)
  ;; If the function is closed over, in general we cannot conclude it's
  ;; called simply - as it could be stored somewhere and called later, etc.
  ;; However there are a few special cases we can handle.
  (let ((enclose (bir:enclose function)))
    (when enclose
      (let* ((eout (bir:output enclose))
             (user (bir:use eout)))
        (when (and user
                   (not (simple-user-p client user come-from function)))
          (return-from function-called-simply-p nil)))))
  ;; Now check over the local calls. They must all be simple.
  (set:doset (call (bir:local-calls function) t)
    (unless (simple-user-p client call come-from function)
      (return nil))))

(defun simple-instruction-p (client inst dest)
  (simple-dynenv-p client (bir:dynamic-environment inst) dest))

(defgeneric simple-unwinding-p (client instruction))
(defmethod simple-unwinding-p (client (unwind bir:unwind))
  (let ((*seen* (set:empty-set)))
    (simple-instruction-p client unwind (bir:come-from unwind))))

(defmethod simple-unwinding-p (client (inst bir:come-from))
  (set:every (lambda (unw) (simple-unwinding-p client unw))
             (bir:unwinds inst)))
