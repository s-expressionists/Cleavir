(cl:in-package #:asdf-user)

(defsystem :cleavir-bir-to-cl
  :depends-on (:cleavir-bir)
  :components
  ((:file "packages")
   (:file "translate" :depends-on ("packages"))))
