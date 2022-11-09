(cl:in-package #:asdf-user)

(defsystem #:cleavir-cst-to-ast-test
  :depends-on (#:cleavir-cst-to-ast
               #:cleavir-ast-transformations
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
        (funcall thunk)))))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:cleavir-cst-to-ast-test '#:test)))
