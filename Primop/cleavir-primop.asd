(cl:in-package #:asdf-user)

(defsystem :cleavir-primop
  :description "Abstract Syntax Tree representation for Common Lisp code."
  :author ("Robert Strandh <robert.strandh@gmail.com>"
           "Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/cleavir-primop/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :depends-on (:cleavir-attributes)
  :components
  ((:file "packages")
   (:file "info" :depends-on ("packages"))
   (:file "definitions" :depends-on ("packages" "info"))))
