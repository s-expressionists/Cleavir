(cl:in-package #:asdf-user)

(defsystem :cleavir-def-use-chains-test
  :depends-on (:cleavir-def-use-chains
	       :cleavir-graph-test-utilities)
  :serial t
  :components
  ((:file "test-packages")
   (:file "test-def-use-chains")))
