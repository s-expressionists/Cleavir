(cl:in-package #:asdf-user)

;;;; Conceptual summary:
(defsystem :cleavir-compilation-policy
  :description "System for representing and interrogating compiler policies."
  :author ("Robert Strandh <robert.strandh@gmail.com>"
           "Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/cleavir-compilation-policy/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :depends-on (:cleavir-environment :acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "policy")
   (:file "define-policy")
   (:file "optimize")
   (:file "compute")))
