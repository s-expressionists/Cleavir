(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT-CONSTANT is called when a constant is found, either in the
;;; form of a literal or in the form of a constant variable.

(defun convert-constant (constant-cst env system)
  (declare (ignore env system))
  (ast:make-constant-ast (cst:raw constant-cst)
    :origin (cst:source constant-cst)))
