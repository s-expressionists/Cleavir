(in-package #:cleavir-documentation-generation)

(defmethod staple:packages ((sys (eql (asdf:find-system :cleavir-meter))))
  (list (find-package "CLEAVIR-METER")))

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-meter))))
  'cleavir-page)
