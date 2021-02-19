(cl:in-package #:asdf-user)

(defsystem #:cleavir-cst-to-ast-test
  :depends-on (#:cleavir-cst-to-ast
               #:cleavir-io)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "ast-from-string")
   (:file "ast-equal-p")
   (:file "assign-sources")
   #+dont (:file "test"
    :around-compile
    (lambda (thunk)
      (progv (list '*readtable*
                   (find-symbol "*POLICY*" (find-package "CLEAVIR-AST")))
             (list (symbol-value (find-symbol "*IO-READTABLE*" (find-package "CLEAVIR-IO")))
                   nil)
        (funcall thunk))))))
