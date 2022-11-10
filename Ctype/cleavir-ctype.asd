(cl:in-package #:asdf-user)

(defsystem :cleavir-ctype
  :description "Interface to client type systems."
  :author ("Bike <aeshtaer@gmail.com>"
           "Charles Zhang")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :depends-on (:cleavir-io
               :cleavir-attributes
	       :cleavir-meter)

  :depends-on ()
  :components
  ((:file "packages")
   (:file "generic-functions" :depends-on ("packages"))
   (:file "other-functions" :depends-on ("packages"))
   (:file "default" :depends-on ("generic-functions"
                                 "packages"))))
