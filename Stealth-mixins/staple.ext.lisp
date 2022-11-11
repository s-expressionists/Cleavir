(in-package #:cleavir-documentation-generation)

(defmethod staple:packages ((sys (eql (asdf:find-system :cleavir-stealth-mixins))))
  (list (find-package "CLEAVIR-STEALTH-MIXINS")))

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-stealth-mixins))))
  'cleavir-page)
