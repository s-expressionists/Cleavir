(cl:in-package #:asdf-user)

(defsystem #:cleavir-conditions
  :description "Utilities for conditions signaled by compilers."
  :author "Bike <aeshtaer@gmail.com>"
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :depends-on (:acclimation)
  :components
  ((:file "packages")
   (:file "program-condition" :depends-on ("packages"))
   (:file "origin" :depends-on ("packages"))
   (:file "note" :depends-on ("program-condition" "packages"))))
