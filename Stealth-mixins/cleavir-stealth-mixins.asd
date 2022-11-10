(cl:in-package #:asdf-user)

(defsystem :cleavir-stealth-mixins
  :description "Utilities for adding mixins to classes after they are defined."
  :author "Robert Strandh <robert.strandh@gmail.com>"
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/cleavir-stealth-mixins/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :depends-on (:closer-mop)
  :serial t
  :components
  ((:file "packages")
   (:file "stealth-mixins")))
