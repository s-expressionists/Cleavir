(cl:in-package #:asdf-user)

(defsystem :cleavir-ast
  :depends-on (:cleavir-io
               :cleavir-attributes
	       :cleavir-meter)
  :serial t
  :components
  ((:file "packages")
   (:file "general-purpose-asts")
   (:file "graphviz-drawing")
   (:file "map-ast")))
