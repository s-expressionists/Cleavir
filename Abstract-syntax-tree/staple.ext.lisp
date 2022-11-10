(in-package #:cleavir-documentation-generation)

(defmethod staple:page-type ((sys (eql (asdf:find-system :cleavir-ast))))
  'cleavir-page)

(defmethod staple:subsystems ((sys (eql (asdf:find-system :cleavir-ast))))
  ;; Without this method, Staple will think cleavir-ast-to-bir is a subsystem, since its
  ;; name starts with "cleavir-ast".
  ())
