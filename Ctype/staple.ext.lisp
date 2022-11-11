(in-package #:cleavir-documentation-generation)

(defmethod staple:packages ((sys (eql (asdf:find-system :cleavir-ctype))))
  (list (find-package "CLEAVIR-CTYPE")))

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-ctype))))
  'cleavir-page)
