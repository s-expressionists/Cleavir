(cl:in-package #:asdf-user)

(defsystem :cleavir-abstract-interpreter
  :depends-on (:cleavir-bir :cleavir-set
               :cleavir-attributes :cleavir-ctype)
  :components
  ((:file "packages")
   (:file "association" :depends-on ("packages"))
   (:file "domain" :depends-on ("packages"))
   (:file "values" :depends-on ("domain" "packages"))
   (:file "interpreter" :depends-on ("packages"))
   (:file "control" :depends-on ("interpreter" "domain"))
   (:file "data" :depends-on ("interpreter" "domain" "packages"))
   (:file "values-data" :depends-on ("interpreter" "domain" "values"
                                                   "packages"))
   (:file "type" :depends-on ("values-data" "packages"))))
