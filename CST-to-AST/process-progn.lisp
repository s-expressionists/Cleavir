(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Turn a list of ASTs into either a PROGN-AST, the unique AST in the
;;; list if it has only one AST, or a CONSTANT-AST containing NIL in
;;; case the list of ASTs is NIL.

(defun process-progn (asts &optional origin)
  (cond ((null asts) (ast:make-constant-ast nil :origin origin))
        ((null (rest asts)) (first asts))
        (t (ast:make-progn-ast asts :origin origin))))
