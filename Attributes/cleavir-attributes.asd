(in-package #:asdf-user)

(defsystem :cleavir-attributes
  :description "Information about values not encompassed by the type system."
  :author ("Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/cleavir-attributes/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :depends-on (:cleavir-io)
  :components
  ((:file "packages")
   (:file "flags" :depends-on ("packages"))
   (:file "attributes" :depends-on ("flags" "packages"))))
