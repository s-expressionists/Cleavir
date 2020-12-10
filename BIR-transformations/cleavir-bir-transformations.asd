(cl:in-package #:asdf-user)

(defsystem :cleavir-bir-transformations
  :depends-on (:cleavir-bir :cleavir-set
               :cleavir-attributes :cleavir-ctype)
  :components
  ((:file "packages")
   (:file "eliminate-catches" :depends-on ("packages"))
   (:file "process-captured-variables" :depends-on ("packages"))
   (:file "delete-temporary-variables" :depends-on ("packages"))
   (:file "interpolate-function" :depends-on ("eliminate-catches"
                                              "packages"))
   (:file "copy-function" :depends-on ("packages"))
   (:file "inline" :depends-on ("interpolate-function" "packages"))
   (:file "simple-unwind" :depends-on ("packages"))
   (:file "meta-evaluate" :depends-on ("packages"))
   (:file "generate-type-checks" :depends-on ("packages"))))
