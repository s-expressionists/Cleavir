(in-package #:cleavir-documentation-generation)

(defmethod staple:packages ((sys (eql (asdf:find-system :cleavir-cst-to-ast))))
  (list (find-package "CLEAVIR-CST-TO-AST")))

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-cst-to-ast))))
  'cleavir-page)
