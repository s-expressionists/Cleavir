(cl:in-package #:asdf-user)

(defsystem :cleavir-bir
  :depends-on (:cleavir-primop)
  :components
  ((:file "packages")
   (:file "set" :depends-on ("packages"))
   (:file "structure" :depends-on ("set" "packages"))
   (:file "primops" :depends-on ("structure" "packages"))
   (:file "instructions" :depends-on ("primops" "set" "structure" "packages"))
   (:file "map" :depends-on ("instructions" "set" "structure" "packages"))
   (:file "graph-modifications"
    :depends-on ("map" "instructions" "set" "structure" "packages"))
   (:file "verify"
    :depends-on ("map" "instructions" "set" "structure" "packages"))
   (:file "disassemble"
    :depends-on ("map" "instructions" "set" "structure" "packages"))))
