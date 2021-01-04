(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-variable (cst environment system)
  (convert-cst cst (variable-info environment cst) environment system))
