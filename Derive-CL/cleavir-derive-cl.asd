(defsystem #:cleavir-derive-cl
  :depends-on (#:cleavir-ctype #:cleavir-domain)
  :components
  ((:file "packages")
   (:file "derive" :depends-on ("packages"))
   (:file "aux" :depends-on ("packages"))
   (:module "Type"
    :depends-on ("aux" "derive" "packages")
    :components ((:file "data")
                 (:file "numbers")
                 (:file "arrays")
                 (:file "strings")
                 (:file "sequences")))
   (:file "bits-used" :depends-on ("aux" "derive" "packages"))))
