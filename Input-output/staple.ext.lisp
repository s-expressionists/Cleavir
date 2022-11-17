(in-package #:cleavir-documentation-generation)

(defmethod staple:packages ((sys (eql (asdf:find-system :cleavir-io))))
  (list (find-package "CLEAVIR-IO")))

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-io))))
  'cleavir-page)
