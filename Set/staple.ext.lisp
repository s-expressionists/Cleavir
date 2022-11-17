(in-package #:cleavir-documentation-generation)

(defmethod staple:packages ((sys (eql (asdf:find-system :cleavir-set))))
  (list (find-package "CLEAVIR-SET")))

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-set))))
  'cleavir-page)
