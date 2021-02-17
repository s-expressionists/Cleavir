(cl:in-package #:asdf-user)

(defsystem :cleavir-liveness
  :depends-on (:cleavir-flow :cleavir-graph :cleavir-set)
  :components
  ((:file "packages")
   (:file "liveness" :depends-on ("packages"))))
