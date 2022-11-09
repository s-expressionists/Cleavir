(in-package #:cleavir-documentation-generation)

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-set))))
  'cleavir-page)
