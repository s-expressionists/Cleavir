(cl:in-package #:asdf-user)

(defsystem :cleavir-set
  :depends-on ()
  :components
  ((:file "packages")
   (:file "set" :depends-on ("packages"))))
