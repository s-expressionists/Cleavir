(defsystem "cleavir-bir-visualizer"
  :description "A simple interactive visualizer for the BIR intermediate representation"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     "0.1"
  :depends-on  ("alexandria"

                "eclector-concrete-syntax-tree"

                "cleavir-cst-to-ast"
                "cleavir-ast-to-bir"
                "cleavir-bir"
                "cleavir-bir-transformations"

                "mcclim"
                "clouseau")
  :serial t
  :components  ((:file "package")
                (:file "compile")
                (:file "inspect")
                (:file "application")))
