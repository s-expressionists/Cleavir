(cl:in-package #:cleavir-cst-to-ast)

;;; ENV is an environment that is known to contain information about
;;; the variable VARIABLE, but we don't know whether it is special or
;;; lexical.  VALUE-AST is an AST that computes the value to be given
;;; to VARIABLE.  NEXT-AST is an AST that represents the computation
;;; to take place after the variable has been given its value.  If the
;;; variable is special, this function creates a BIND-AST with
;;; NEXT-AST as its body.  If the variable is lexical, this function
;;; creates a PROGN-AST with two ASTs in it.  The first one is a
;;; LEXICAL-BIND-AST that assigns the value to the variable, and the second
;;; one is the NEXT-AST.
(defun set-or-bind-variable (variable-cst value-ast next-ast env system)
  (let* ((info (env:variable-info system env (cst:raw variable-cst)))
         (_ (assert (not (null info))))
         ;; Type wrap the value. Per CLHS 3.3.4 "Declaration Scope"
         ;; bound declarations do apply to the initial value of the binding.
         ;; (The page on the TYPE declaration also specifically says it
         ;;  applies to the initial values of bindings.)
         (value-ast
           (type-wrap value-ast (ctype:single-value (env:type info) system)
                      (ast:origin value-ast) env system)))
    (declare (ignore _))
    (if (typep info 'env:special-variable-info)
        (convert-special-binding
         variable-cst value-ast next-ast env system)
	(ast:make-progn-ast
	 (list (ast:make-lexical-bind-ast
		(env:identity info)
		value-ast
                :origin variable-cst
                :ignore (env:ignore info))
	       next-ast)
         :origin variable-cst))))
