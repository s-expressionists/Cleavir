(defsystem #:cleavir-domain
  :depends-on (#:cleavir-ctype #:cleavir-attributes)
  :components
  ((:file "packages")
   (:file "domain")
   (:file "values")
   (:file "product")
   (:file "derive")
   (:module "Domains"
    :depends-on ("packages")
    :components ((:file "type")
                 (:file "attribute")
                 (:file "bits-used")
                 (:file "equivalence")
                 (:file "reachability")))))
