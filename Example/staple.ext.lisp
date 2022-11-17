(in-package #:cleavir-documentation-generation)

(defmethod staple:packages ((sys (eql (asdf:find-system :cleavir-example))))
  (list (find-package "CLEAVIR-EXAMPLE")))

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-example))))
  'cleavir-page)
