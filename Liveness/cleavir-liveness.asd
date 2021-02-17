(cl:in-package #:asdf-user)

(defsystem :cleavir-liveness
  :depends-on (:cleavir-flow :cleavir-graph)
  :components
  ((:file "packages")
   (:file "liveness" :depends-on ("packages"))))
