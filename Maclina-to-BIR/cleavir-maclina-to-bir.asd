(asdf:defsystem #:cleavir-maclina-to-bir
  :description "Convert Maclina bytecode to Cleavir IR"
  :author ("Bike <aeshtaer@gmail.com>")
  :depends-on (:maclina/base
               :cleavir-bir-builder :cleavir-compilation-policy)
  :components ((:file "maclina-to-bir")))

(asdf:defsystem #:cleavir-maclina-to-bir/module
  :description "Convert Maclina compilation modules to Cleavir IR"
  :author ("Bike <aeshtaer@gmail.com>")
  :depends-on (:cleavir-maclina-to-bir :maclina/compile)
  :components ((:file "compile")))
