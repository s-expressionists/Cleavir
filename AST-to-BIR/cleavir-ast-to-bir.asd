(cl:in-package #:asdf-user)

(defsystem :cleavir-ast-to-bir
  :description "Compiler of abstract syntax trees into BIR."
  :author ("Bike <aeshtaer@gmail.com>"
           "Charles Zhang")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :depends-on (:cleavir-ast :cleavir-bir :cleavir-primop :cleavir-ctype)
  :components
  ((:file "packages")
   (:file "infrastructure" :depends-on ("packages"))
   (:file "compile-general-purpose-asts"
    :depends-on ("infrastructure" "packages"))
   (:file "compile-multiple-value-related-asts"
    :depends-on ("infrastructure" "packages"))
   (:file "compile-primops" :depends-on ("infrastructure" "packages"))))
