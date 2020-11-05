(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Turn a list of ASTs into either a PROGN-AST or a
;;; LOAD-TIME-VALUE-AST containing NIL in case the list of ASTs is
;;; NIL.

(defun process-progn (asts &optional origin)
  (if (null asts)
      (cleavir-ast:make-constant-ast nil :origin origin)
      (cleavir-ast:make-progn-ast asts :origin origin)))
