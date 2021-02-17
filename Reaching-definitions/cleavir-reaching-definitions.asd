(cl:in-package #:asdf-user)

(defsystem :cleavir-reaching-definitions
  :depends-on (:cleavir-flow :cleavir-graph)
  :serial t
  :components
  ((:file "packages")
   (:file "reaching-definitions")))
