(cl:in-package #:asdf-user)

(defsystem :cleavir-example
  :depends-on (:cleavir-ctype :cleavir-environment :cleavir-compilation-policy
                              :cleavir-primop
               :concrete-syntax-tree)
  :components
  ((:file "packages")
   (:file "system" :depends-on ("packages"))
   (:file "environment" :depends-on ("system" "packages"))
   (:file "macros" :depends-on ("packages"))
   (:file "load-environment" :depends-on ("macros" "environment"
                                                   "packages"))))
