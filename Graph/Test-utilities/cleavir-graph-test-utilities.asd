(asdf:defsystem :cleavir-graph-test-utilities
  :depends-on (:cleavir-graph)
  :components
  ((:file "packages" :depends-on ())
   (:file "test-utilities" :depends-on ("packages"))))
