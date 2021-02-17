(asdf:defsystem :cleavir-graph
  :depends-on ()
  :components
  ((:file "package" :depends-on ())
   (:file "graph" :depends-on ("package"))
   (:file "defaults" :depends-on ("graph" "package"))))
