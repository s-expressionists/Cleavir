(in-package #:cleavir-documentation-generation)

(defmethod staple:packages ((sys (eql (asdf:find-system :cleavir-primop))))
  (mapcar #'find-package '("CLEAVIR-PRIMOP-INFO" "CLEAVIR-PRIMOP")))

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-primop))))
  'cleavir-page)
