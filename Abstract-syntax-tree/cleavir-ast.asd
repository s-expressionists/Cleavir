(cl:in-package #:asdf-user)

(defsystem :cleavir-ast
  :description "Abstract Syntax Tree representation for Common Lisp code."
  :author ("Robert Strandh <robert.strandh@gmail.com>"
           "Bike <aeshtaer@gmail.com>"
           "Charles Zhang")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/cleavir-ast/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :depends-on (:cleavir-io
               :cleavir-attributes)
  :serial t
  :components
  ((:file "packages")
   (:file "general-purpose-asts")
   (:file "graphviz-drawing")
   (:file "map-ast")))
