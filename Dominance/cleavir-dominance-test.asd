(cl:in-package #:asdf-user)

(defsystem :cleavir-dominance-test
  :depends-on (:cleavir-graph :cleavir-graph-test-utilities
	       :cleavir-dominance)
  :components
  ((:file "test-packages" :depends-on ())
   (:file "test-dominance" :depends-on ("test-packages"))))
