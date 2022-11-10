(cl:in-package #:asdf-user)

(defsystem :cleavir-set
  :description "A set data structure."
  :author ("Bike <aeshtaer@gmail.com>" "Charles Zhang")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :depends-on ()
  :components
  ((:file "packages")
   (:file "set" :depends-on ("packages"))))
