(cl:in-package #:asdf-user)

(defsystem :cleavir-bir-transformations
  :depends-on (:cleavir-bir :cleavir-set)
  :components
  ((:file "packages")
   (:file "function-dag" :depends-on ("packages"))
   (:file "process-captured-variables"
    :depends-on ("function-dag" "packages"))
   (:file "delete-temporary-variables"
    :depends-on ("packages"))))
