(cl:in-package #:asdf-user)

(defsystem :cleavir-bir-transformations
  :depends-on (:cleavir-bir :cleavir-set
               :cleavir-attributes) ; for simple-unwind
  :components
  ((:file "packages")
   (:file "process-captured-variables" :depends-on ("packages"))
   (:file "delete-temporary-variables" :depends-on ("packages"))
   (:file "interpolate-function" :depends-on ("packages"))
   (:file "inline" :depends-on ("interpolate-function" "packages"))
   (:file "simple-unwind" :depends-on ("packages"))))
