(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-special-binding
    (client variable-cst value-ast next-ast env)
  (declare (ignore client env))
  (make-instance 'ast:constant-dynamic-bind-ast
    :name (cst:raw variable-cst)
    :value-ast value-ast
    :body-ast next-ast
    :origin variable-cst))
