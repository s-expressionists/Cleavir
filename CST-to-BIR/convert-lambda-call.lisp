(in-package #:cleavir-cst-to-bir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form when the head of a compound form is a
;;; CONS.  Then the head must be a lambda expression.  We replace a
;;; call such as ((lambda (params) . body) . args) by (flet ((temp
;;; (params) . body)) (temp . args))
;;;
;;; FIXME: do some more error checking.

(defmethod convert-lambda-call (cst inserter env system)
  (cst:db origin ((lambda-cst lambda-list-cst . body-cst) . args-cst) cst
    (assert (eql (cst:raw lambda-cst) 'cl:lambda) nil
            'lambda-call-first-symbol-not-lambda :cst lambda-cst)
    ;; We can compile the arguments first since the lambda can't have
    ;; any side effects. If any of the argument forms never return, we
    ;; don't need to bother compiling the lambda at all.
    ;; TODO? Compile directly to a local call.
    (with-compiled-arguments (args args-cst inserter env system)
      (let ((callee (convert-function
                     lambda-list-cst body-cst inserter env system
                     :origin origin))
            (call-out (make-instance 'bir:output)))
        (insert inserter (make-instance 'bir:call
                           :inputs (list* callee args)
                           :outputs (list call-out)))
        (list call-out)))))
