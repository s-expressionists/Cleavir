(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-special-binding
    (variable-cst value-ast next-ast env system)
  (declare (ignore env system))
  (make-instance 'ast:constant-dynamic-bind-ast
    :name (cst:raw variable-cst)
    :value-ast value-ast
    :body-ast next-ast
    :origin variable-cst))
