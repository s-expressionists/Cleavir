(cl:in-package #:asdf-user)

(defsystem :cleavir-example
  :description "Example frontend for the Cleavir toolkit."
  :author "Bike <aeshtaer@gmail.com>"
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/cleavir-example/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :depends-on (:cleavir-ctype :cleavir-environment :cleavir-compilation-policy
                              :cleavir-primop
               :cleavir-cst-to-ast :cleavir-ast-to-bir
               :cleavir-bir-transformations
               :cleavir-abstract-interpreter
               :concrete-syntax-tree
               :ctype :ctype/tfun)
  :components
  ((:file "packages")
   (:file "system" :depends-on ("packages"))
   (:file "environment" :depends-on ("system" "packages"))
   (:file "environment-interface" :depends-on ("environment" "system"
                                                             "packages"))
   (:file "fold" :depends-on ("environment-interface" "system" "packages"))
   (:file "type" :depends-on ("system" "packages"))
   (:file "derive-type" :depends-on ("system" "packages"))
   (:file "macros" :depends-on ("packages"))
   (:file "load-environment" :depends-on ("macros" "environment"
                                                   "packages"))
   (:file "compile" :depends-on ("system" "environment" "packages"))))
