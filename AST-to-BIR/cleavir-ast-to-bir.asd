(cl:in-package #:asdf-user)

(defsystem :cleavir-ast-to-bir
  :depends-on (:cleavir-ast :cleavir-bir :cleavir-primop :cleavir-ctype)
  :components
  ((:file "packages")
   (:file "infrastructure" :depends-on ("packages"))
   (:file "compile-general-purpose-asts"
    :depends-on ("infrastructure" "packages"))
   (:file "compile-multiple-value-related-asts"
    :depends-on ("infrastructure" "packages"))
   (:file "compile-primops" :depends-on ("infrastructure" "packages"))
   (:file "compile-fixnum-related-asts"
    :depends-on ("compile-primops" "infrastructure" "packages"))))
