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

(defgeneric simple-dynenv-p (dynenv dest system)
  (:method ((dynenv bir:dynamic-environment) (dest bir:dynamic-environment)
            system)
    (declare (ignore system))
    (eq dynenv dest)))

(defmethod simple-dynenv-p ((dynenv bir:function)
                            (dest bir:dynamic-environment)
                            system)
  (function-called-simply-p dynenv dest system))

(defgeneric simple-user-p (user dest datum system)
  (:method ((user bir:instruction) (dest bir:dynamic-environment)
            datum system)
    (declare (ignore datum system))
    nil))

(defmethod simple-user-p ((user bir:abstract-call)
                          (dest bir:dynamic-environment)
                          datum system)
  (and (or (eq (bir:callee user) datum)
           (attributes:has-flag-p (bir:attributes user) :dyn-call))
       (simple-instruction-p user dest system)))

(defmethod simple-user-p ((user bir:abstract-local-call)
                          (dest bir:dynamic-environment)
                          (datum bir:function)
                          system)
  (and (eq (bir:callee user) datum)
       (simple-instruction-p user dest system)))

(defun function-called-simply-p (function come-from system)
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
                   (not (simple-user-p user come-from function system)))
          (return-from function-called-simply-p nil)))))
  ;; Now check over the local calls. They must all be simple.
  (set:doset (call (bir:local-calls function) t)
    (unless (simple-user-p call come-from function system)
      (return nil))))

(defun simple-instruction-p (inst dest system)
  (simple-dynenv-p (bir:dynamic-environment inst) dest system))

(defgeneric simple-unwinding-p (instruction system))
(defmethod simple-unwinding-p ((unwind bir:unwind) system)
  (let ((*seen* (set:empty-set)))
    (simple-instruction-p unwind (bir:come-from unwind) system)))

(defmethod simple-unwinding-p ((inst bir:come-from) system)
  (set:every (lambda (unw) (simple-unwinding-p unw system))
             (bir:unwinds inst)))
