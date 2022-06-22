(cl:in-package #:asdf-user)

(defsystem :cleavir-abstract-interpreter
  :depends-on (:cleavir-bir :cleavir-set :cleavir-stealth-mixins
               :cleavir-attributes :cleavir-ctype)
  :components
  ((:file "packages")
   (:file "domain" :depends-on ("packages"))
   (:file "strategy" :depends-on ("packages"))
   (:file "values" :depends-on ("domain" "packages"))
   (:file "interpret-gfs" :depends-on ("packages"))
   (:file "product" :depends-on ("interpret-gfs" "domain" "packages"))
   (:file "interpret" :depends-on ("product" "interpret-gfs" "packages"))
   (:file "sequential" :depends-on ("product" "interpret" "packages"))
   (:file "control" :depends-on ("interpret-gfs" "domain" "packages"))
   (:file "reachability" :depends-on ("control" "domain" "packages"))
   (:file "data" :depends-on ("interpret-gfs" "domain" "packages"))
   (:file "values-data" :depends-on ("data" "values" "packages"))
   (:file "attribute" :depends-on ("values-data" "packages"))
   (:file "known-call" :depends-on ("attribute" "product" "values-data" "packages"))
   (:file "type" :depends-on ("values-data" "interpret-gfs" "packages"))
   (:file "use" :depends-on ("values-data" "interpret-gfs" "packages"))
   (:file "reachable-data" :depends-on ("product" "reachability" "data" "packages"))
   (:file "typed-reachability" :depends-on ("product" "type" "reachability" "packages"))
   (:file "slots" :depends-on ("sequential" "attribute" "type" "interpret-gfs"
                                            "packages"))))
