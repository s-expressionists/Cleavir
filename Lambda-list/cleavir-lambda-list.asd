(defsystem :cleavir-lambda-list
  :depends-on ()
  :components ((:file "packages")
               (:file "generic-functions" :depends-on ("packages"))
               (:file "default" :depends-on ("generic-functions" "packages"))
               (:file "map" :depends-on ("generic-functions" "packages"))))
