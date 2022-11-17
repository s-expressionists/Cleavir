(in-package #:cleavir-documentation-generation)

(defmethod staple:packages ((sys (eql (asdf:find-system :cleavir-environment))))
  (list (find-package "CLEAVIR-ENVIRONMENT")))

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-environment))))
  'cleavir-page)
