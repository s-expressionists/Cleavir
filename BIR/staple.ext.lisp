(in-package #:cleavir-documentation-generation)

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-bir))))
  'cleavir-page)

(defmethod staple:template ((sys (eql (asdf:find-system :cleavir-bir))))
  (asdf:system-relative-pathname (asdf:find-system :cleavir-documentation-generation)
                                 "main" :type "ctml"))

(defmethod staple:subsystems ((sys (eql (asdf:find-system :cleavir-bir))))
  (list (asdf:find-system :cleavir-bir-visualizer)))

(defmethod staple:images ((sys (eql (asdf:find-system :cleavir-bir))))
  ;; Make sure none of the images from the visualizer are duplicated here.
  ())

(defmethod staple:packages ((sys (eql (asdf:find-system :cleavir-bir))))
  (mapcar #'find-package '("CLEAVIR-BIR" "CLEAVIR-BIR-DISASSEMBLER")))

(defmethod staple:documents ((sys (eql (asdf:find-system :cleavir-bir))))
  (list (asdf:system-relative-pathname sys "README" :type "md")))
