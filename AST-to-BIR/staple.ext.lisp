(in-package #:cleavir-documentation-generation)

(defmethod staple:packages ((sys (eql (asdf:find-system :cleavir-ast-to-bir))))
  (list (find-package "CLEAVIR-AST-TO-BIR")))

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-ast-to-bir))))
  'cleavir-page)
