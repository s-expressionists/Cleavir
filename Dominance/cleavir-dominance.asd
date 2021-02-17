(cl:in-package #:asdf-user)

(defsystem :cleavir-dominance
  :depends-on (:cleavir-graph :cleavir-meter)
  :serial t
  :components
  ((:file "packages")
   (:file "dominance")))
