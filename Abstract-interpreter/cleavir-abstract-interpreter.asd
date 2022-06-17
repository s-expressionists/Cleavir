(cl:in-package #:asdf-user)

(defsystem :cleavir-abstract-interpreter
  :depends-on (:cleavir-bir :cleavir-set
               :cleavir-attributes :cleavir-ctype)
  :components
  ((:file "packages")
   (:file "domain" :depends-on ("packages"))
   (:file "product" :depends-on ("packages"))
   (:file "values" :depends-on ("domain" "packages"))
   (:file "interpret" :depends-on ("packages"))
   (:file "sequential" :depends-on ("product" "interpret" "packages"))
   (:file "control" :depends-on ("product" "interpret" "domain" "packages"))
   (:file "reachability" :depends-on ("product" "control" "domain" "packages"))
   (:file "data" :depends-on ("reachability" "product" "interpret" "domain"
                                             "packages"))
   (:file "values-data" :depends-on ("interpret" "domain" "values" "packages"))
   (:file "attribute" :depends-on ("values-data" "packages"))
   (:file "known-call" :depends-on ("attribute" "values-data" "packages"))
   (:file "type" :depends-on ("known-call" "values-data" "interpret" "packages"))
   (:file "typed-reachability" :depends-on ("type" "reachability" "packages"))
   (:file "slots" :depends-on ("sequential" "attribute" "type" "interpret"
                                            "packages"))))
