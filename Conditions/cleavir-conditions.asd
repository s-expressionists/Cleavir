(cl:in-package #:asdf-user)

(defsystem #:cleavir-conditions
  :depends-on (:acclimation)
  :components
  ((:file "packages")
   (:file "program-condition" :depends-on ("packages"))
   (:file "origin" :depends-on ("packages"))
   (:file "note" :depends-on ("program-condition" "packages"))))
