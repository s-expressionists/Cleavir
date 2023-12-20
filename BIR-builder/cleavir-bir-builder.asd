(defsystem :cleavir-bir-builder
  :description "Helper system for constructing BIR."
  :author ("Bike <aeshtaer@gmail.com>" "Charles Zhang")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/cleavir-bir-builder/"
  :version "1.0.0"
  :license "BSD"
  :bug-tracker "https://github.com/s-expressionists/Cleavir/issues"
  :source-control (:git "https://github.com/s-expressionists/Cleavir.git")
  :depends-on (:cleavir-bir)
  :components ((:file "packages")
               (:file "builder" :depends-on ("packages"))))
