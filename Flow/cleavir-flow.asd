(asdf:defsystem :cleavir-flow
  :depends-on (:cleavir-graph)
  :components
  ((:file "packages")
   (:file "generic-functions" :depends-on ("packages"))
   (:file "traversal" :depends-on ("generic-functions" "packages"))
   (:file "flow" :depends-on ("generic-functions" "packages"))))
