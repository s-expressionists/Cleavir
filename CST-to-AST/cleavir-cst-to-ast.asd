(cl:in-package #:asdf-user)

(defsystem :cleavir-cst-to-ast
  :description "Compiler of source concrete syntax trees into abstract syntax trees."
  :author ("Robert Strandh <robert.strandh@gmail.com>"
           "Bike <aeshtaer@gmail.com>"
           "Charles Zhang")
  :maintainer ("Bike <aeshtaer@gmail.com>")
  :homepage "https://s-expressionists.github.io/Cleavir/cleavir-cst-to-ast/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :depends-on (:concrete-syntax-tree
               :concrete-syntax-tree-destructuring
               :cleavir-ast
	       :cleavir-ast-transformations
	       :cleavir-primop
	       :cleavir-environment
	       :cleavir-compilation-policy
               :cleavir-ctype
               :cleavir-conditions
               :acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "environment-augmentation")
   (:file "environment-query")
   (:file "variables")
   (:file "generic-functions")
   (:file "convert-function-reference")
   (:file "convert-special-binding")
   (:file "utilities")
   (:file "set-or-bind-variable")
   (:file "process-progn")
   (:file "convert-sequence")
   (:file "convert-variable")
   (:file "convert")
   (:file "process-init-parameter")
   (:file "itemize-declaration-specifiers")
   (:file "itemize-lambda-list")
   (:file "lambda-list-from-parameter-groups")
   (:file "convert-setq")
   (:file "convert-let-and-letstar")
   (:file "convert-code")
   (:file "convert-lambda-call")
   (:file "convert-constant")
   (:file "convert-special")
   (:file "convert-primop")
   (:file "convert-cst")
   (:file "cst-to-ast")))
