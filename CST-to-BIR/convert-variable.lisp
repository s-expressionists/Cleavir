(in-package #:cleavir-cst-to-bir)

(defmethod convert-variable (cst inserter environment system)
  (convert-cst cst (variable-info system environment cst) inserter environment system))
