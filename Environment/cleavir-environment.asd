(cl:in-package #:asdf-user)

(defsystem :cleavir-environment
  :description "Interface for managing lexical environments."
  :author ("Robert Strandh <robert.strandh@gmail.com>" "Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :depends-on (:acclimation :cleavir-ctype :cleavir-attributes)
  :serial t
  :components
  ((:file "packages")
   (:file "query")
   (:file "augmentation-functions")
   (:file "default-augmentation-classes")
   (:file "compile-time")
   (:file "optimize-qualities")
   (:file "declarations")
   (:file "type-information")
   (:file "default-info-methods")
   (:file "eval")))
