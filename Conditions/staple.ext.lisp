(in-package #:cleavir-documentation-generation)

(defmethod staple:packages ((sys (eql (asdf:find-system :cleavir-conditions))))
  (list (find-package "CLEAVIR-CONDITIONS")))

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-conditions))))
  'cleavir-page)
