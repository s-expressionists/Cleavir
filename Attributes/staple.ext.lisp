(in-package #:cleavir-documentation-generation)

(defmethod staple:packages ((sys (eql (asdf:find-system :cleavir-attributes))))
  (list (find-package "CLEAVIR-ATTRIBUTES")))

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-attributes))))
  'cleavir-page)
