(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-special-binding
    (variable-cst value-ast next-ast env system)
  (make-instance 'ast:dynamic-bind-ast
    :name-ast (convert-constant variable-cst env system)
    :value-ast value-ast
    :body-ast next-ast
    :origin variable-cst))
