(cl:in-package #:asdf-user)

(defsystem :cleavir-meter
  :description "Utilities for measuring performance of compiler tools."
  :author "Robert Strandh <robert.strandh@gmail.com>"
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/cleavir-meter/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :serial t
  :components
  ((:file "packages")
   (:file "meter")))
