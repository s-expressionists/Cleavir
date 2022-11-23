(asdf:defsystem :cleavir-documentation-generation
  :description "Dummy system used for generating Cleavir's documentation."
  ;; I (Bike) am so far the only author, but Staple uses the authors on the front page.
  ;; That could be fixed up, but in the meantime, as a bit of a hack, anybody who's
  ;; done substantial work on Cleavir is included here.
  :author ("Robert Strandh <robert.strandh@gmail.com>"
           "Bike <aeshtaer@gmail.com>"
           "Charles Zhang"
           "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
           "Marco Heisig <marco.heisig@fau.de>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :homepage "https://s-expressionists.github.io/Cleavir/"
  ;; similar to above hack, this is used as the overall version of Cleavir.
  :version (:read-file-form "../version.sexp")
  :license "BSD")
