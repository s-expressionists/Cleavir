(in-package #:cleavir-documentation-generation)

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-ast))))
  'cleavir-page)
