(cl:in-package #:asdf-user)

(defsystem :cleavir-liveness
  :depends-on ()
  :components
  ((:file "packages")
   (:file "liveness" :depends-on ("packages"))))
