(cl:in-package #:asdf-user)

(defsystem :cleavir-abstract-interpreter
  :description "Abstract interpreter for BIR."
  :author "Bike <aeshtaer@gmail.com>"
  :version "0.0.1"
  :license "BSD"
  :depends-on (:cleavir-bir :cleavir-set :cleavir-stealth-mixins
               :cleavir-attributes :cleavir-ctype)
  :components
  ((:file "packages")
   (:file "strategy" :depends-on ("packages"))
   (:file "interpret-gfs" :depends-on ("packages"))
   (:file "interpret" :depends-on ("interpret-gfs" "packages"))
   (:file "sequential" :depends-on ("interpret" "packages"))
   (:file "control" :depends-on ("interpret-gfs" "domain" "packages"))
   (:file "data" :depends-on ("interpret-gfs" "domain" "packages"))
   (:file "values-data" :depends-on ("data" "values" "packages"))))
