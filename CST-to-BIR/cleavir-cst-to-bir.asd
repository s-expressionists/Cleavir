(cl:in-package #:asdf-user)

(defsystem :cleavir-cst-to-bir
  :depends-on (:concrete-syntax-tree
               :concrete-syntax-tree-destructuring
               :cleavir-bir
	       :cleavir-primop
	       :cleavir-environment
	       :cleavir-compilation-policy
               :cleavir-ctype
               :cleavir-conditions
               :acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions" :depends-on ("packages"))
   (:file "condition-reporters-english" :depends-on ("conditions" "packages"))
   (:file "environment-augmentation" :depends-on ("conditions" "packages"))
   (:file "environment-query" :depends-on ("conditions" "packages"))
   (:file "generic-functions" :depends-on ("packages"))
   (:file "convert-function-reference"
    :depends-on ("generic-functions" "conditions" "packages"))
   (:file "utilities" :depends-on ("conditions" "packages"))
   (:file "infrastructure" :depends-on ("packages"))
   (:file "convert-constant" :depends-on ("infrastructure" "packages"))
   (:file "bind-variable"
    :depends-on ("generic-functions" "convert-constant"
                                     "infrastructure" "packages"))
   (:file "convert-sequence"
    :depends-on ("convert-constant" "generic-functions" "packages"))
   (:file "convert-variable"
    :depends-on ("environment-query" "generic-functions" "packages"))
   (:file "convert"
    :depends-on ("environment-query" "convert-constant"
                                     "generic-functions" "packages"))
   (:file "process-init-parameter"
    :depends-on ("generic-functions" "infrastructure"
                                     "bind-variable" "packages"))
   (:file "itemize-declaration-specifiers" :depends-on ("packages"))
   (:file "itemize-lambda-list"
    :depends-on ("generic-functions" "packages"))
   (:file "lambda-list-from-parameter-groups" :depends-on ("packages"))
   (:file "convert-setq"
    :depends-on ("generic-functions" "infrastructure"
                                     "conditions" "packages"))
   (:file "convert-let-and-letstar"
    :depends-on ("generic-functions" "infrastructure" "bind-variable"
                                     "convert-sequence"
                                     "environment-augmentation"
                                     "itemize-declaration-specifiers"
                                     "conditions" "packages"))
   (:file "convert-code"
    :depends-on ("generic-functions" "process-init-parameter"
                                     "environment-augmentation"
                                     "bind-variable" "infrastructure"
                                     "itemize-lambda-list"
                                     "itemize-declaration-specifiers"
                                     "conditions" "packages"))
   (:file "convert-lambda-call"
    :depends-on ("generic-functions" "infrastructure"
                                     "conditions" "packages"))
   (:file "convert-special"
    :depends-on ("generic-functions" "infrastructure" "convert-code"
                                     "convert-sequence"
                                     "conditions" "packages"))
   (:file "convert-primop"
    :depends-on ("generic-functions" "infrastructure" "conditions"
                                     "packages"))
   (:file "convert-cst"
    :depends-on ("generic-functions" "convert-constant"
                                     "infrastructure" "conditions"
                                     "packages"))
   (:file "cst-to-bir"
    :depends-on ("convert-code" "infrastructure" "packages"))
   (:file "toplevel"
    :depends-on ("utilities" "environment-augmentation" "packages"))))
