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

;;; If all unwinds to a given catch are simple, additional
;;; optimizations are possible. The catch instruction does not need
;;; to augment the dynamic environment, as it's only unwound to by
;;; unwinds that don't use the dynamic environment anyway.
;;; (Though indicating its existence to debugging tools may be prudent.)

;;; This code determines whether an unwind can be seen to be
;;; called in the dynamic environment output by their destination.
;;; It can also be used on catch instructions, in which case it checks whether
;;; it's true of all the unwinds to it.

;;; This pass can and probably should be run after inlining.

;;; TODO: Much extension is possible:

;;; In cases where the unwind's dynamic environment has been
;;; altered by the function that's unwinding, this pass does not understand
;;; the unwind as simple. Those unwinds could be made simple by
;;; preceding them with an appropriate local-unwind.
;;; This insertion can and probably should be done back in AST-to-BIR,
;;; not here, since it's valid regardless.

;;; Cases where the call's dynamic environment has been augmented
;;; from the catch may also be possible to handle, but this might
;;; require some changes in the BIR representation of nonlocal exits.

;;; ALSO, might want to cache this information?

;;; Nesting of dynamically safe functions is not understood by this
;;; pass, e.g. the unwind and catch in
;;; (block nil (mapc (lambda (x) (mapc (lambda (y) (return y)) x)) z))
;;; are considered non-simple. This may not happen much in real code
;;; anyway, though, so it may not be worth the effort?

;;; A using-instruction of a function location is amenable to the
;;; simplification process if
;;; a) it's a call to a function that doesn't augment the dynamic
;;;    environment for calls to the function, or
;;; b) it's a call to the function
(defun simplifiable-user-p (user fn)
  (typecase user
    (cleavir-bir:abstract-call
     (or (cleavir-attributes:has-boolean-attribute-p
          (cleavir-bir:attributes user)
          :dyn-call)
         (eq (cleavir-bir:callee user) fn)))
    (t nil)))

(defgeneric simple-unwinding-p (instruction))
(defmethod simple-unwinding-p ((unwind cleavir-bir:unwind))
  (let ((de (cleavir-bir:dynamic-environment unwind))
        (catch (cleavir-bir:catch unwind)))
    (unless (typep de 'cleavir-bir:function)
      (return-from simple-unwinding-p nil))
    (cleavir-set:doset (enclose (cleavir-bir:encloses de) t)
      (let* ((user (cleavir-bir:use enclose))
             (user-de (cleavir-bir:dynamic-environment user)))
        (unless (and (eq user-de catch)
                     (simplifiable-user-p user enclose))
          (return nil))))))

(defmethod simple-unwinding-p ((inst cleavir-bir:catch))
  (cleavir-set:every #'simple-unwinding-p (cleavir-bir:unwinds inst)))
