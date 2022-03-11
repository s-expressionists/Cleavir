(cl:in-package #:asdf-user)

(defsystem :cleavir-abstract-interpreter
  :depends-on (:cleavir-bir :cleavir-set
               :cleavir-attributes :cleavir-ctype)
  :components
  ((:file "packages")
   (:file "association" :depends-on ("packages"))
   (:file "domain" :depends-on ("packages"))
   (:file "values" :depends-on ("domain" "packages"))
   (:file "interpret" :depends-on ("packages"))
   (:file "control" :depends-on ("interpret" "domain"))
   (:file "data" :depends-on ("interpret" "domain" "packages"))
   (:file "values-data" :depends-on ("interpret" "domain" "values"
                                                   "packages"))
   (:file "type" :depends-on ("values-data" "interpret" "packages"))
   (:file "attribute" :depends-on ("values-data" "packages"))))
