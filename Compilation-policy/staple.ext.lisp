(in-package #:cleavir-documentation-generation)

(defmethod staple:packages ((sys (eql (asdf:find-system :cleavir-compilation-policy))))
  (list (find-package "CLEAVIR-COMPILATION-POLICY")))

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-compilation-policy))))
  'cleavir-page)
