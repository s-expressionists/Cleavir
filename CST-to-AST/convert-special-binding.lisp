(cl:in-package #:cleavir-cst-to-ast)

;;; We need to build a CST corresponding to the following expression:
;;;
;;; `(cleavir-primop:call-with-variable-bound
;;;   ',variable
;;;   (cleavir-primop:ast ,value-ast)
;;;   (lambda () (cleavir-primop:ast ,next-ast)))
;;;
(defmethod convert-special-binding
    (variable-cst value-ast next-ast env system)
  (let ((source (cst:source variable-cst)))
    (convert (cst:quasiquote
              source
              (cleavir-primop:call-with-variable-bound
               '(cst:unquote variable-cst)
               (cleavir-primop:ast
                (cst:unquote (make-atom-cst value-ast source)))
               (lambda () (cleavir-primop:ast
                           (cst:unquote (make-atom-cst next-ast source))))))
             env system)))
