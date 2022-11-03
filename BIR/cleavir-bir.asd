(cl:in-package #:asdf-user)

(defsystem :cleavir-bir
  :description "Block-based Intermediate Representation for compiled Lisp code."
  :author ("Bike <aeshtaer@gmail.com>" "Charles Zhang")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :depends-on (:cleavir-primop :cleavir-set :cleavir-attributes
                               :acclimation :cleavir-conditions :cleavir-ctype
               :concrete-syntax-tree)
  :components
  ((:file "packages")
   (:file "structure" :depends-on ("packages"))
   (:file "instructions" :depends-on ("structure" "packages"))
   (:file "map" :depends-on ("instructions" "structure" "packages"))
   (:file "conditions" :depends-on ("packages"))
   (:file "graph-modifications"
    :depends-on ("conditions" "map" "instructions" "structure" "packages"))
   (:file "verify"
    :depends-on ("map" "instructions" "structure" "packages"))
   (:file "disassemble"
    :depends-on ("map" "instructions" "structure" "packages"))
   (:file "condition-reporters-english"
    :depends-on ("disassemble" "conditions" "packages"))))
