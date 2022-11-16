(in-package #:cleavir-documentation-generation)

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-bir-visualizer))))
  'cleavir-page)

(defmethod staple:template ((sys (eql (asdf:find-system :cleavir-bir-visualizer))))
  (asdf:system-relative-pathname (asdf:find-system :cleavir-documentation-generation)
                                 "main" :type "ctml"))

(defmethod staple:images ((sys (eql (asdf:find-system :cleavir-bir-visualizer))))
  (list (asdf:system-relative-pathname sys "screenshot.png")))
