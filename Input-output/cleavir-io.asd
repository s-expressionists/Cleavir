(cl:in-package #:asdf-user)

(defsystem :cleavir-io
  :description "Utilities for textual I/O of Lisp objects."
  :author "Robert Strandh <robert.strandh@gmail.com>"
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/cleavir-io/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :serial t
  :components
  ((:file "packages")
   (:file "io")))
