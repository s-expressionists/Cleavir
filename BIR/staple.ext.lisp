(in-package #:cleavir-documentation-generation)

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-bir))))
  'cleavir-page)

(defmethod staple:subsystems ((sys (eql (asdf:find-system :cleavir-bir))))
  (list (asdf:find-system :cleavir-bir-visualizer)))

(defmethod staple:documents ((sys (eql (asdf:find-system :cleavir-bir))))
  (list (asdf:system-relative-pathname sys "README" :type "md")))
