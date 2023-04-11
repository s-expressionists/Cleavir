(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-variable (client cst environment)
  (convert-cst client cst (describe-variable client environment cst) environment))
