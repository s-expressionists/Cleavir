(cl:in-package #:cleavir-cst-to-ast)

;;; VAR and SUPPLIED-P are LEXICAL-VARIABLEs representing a parameter
;;; variable and its associated SUPPLIED-P variable. If no associated
;;; SUPPLIED-P variable is present in the lambda list then
;;; SUPPLIED-P-CST is NIL.  INIT-AST is the AST that computes the
;;; value to be assigned to the variable represented by VAR-CST if no
;;; argument was supplied for it.  ENV is an environment that already
;;; contains the variables corresponding to VAR-CST and SUPPLIED-P-CST
;;; (if it is not NIL).

;;; This function returns an AST that represents processing of this
;;; parameter and the next computation.
(defun process-init-parameter
    (var-cst var supplied-p-cst supplied-p init-ast env next-ast system)
  (let* ((origin (cst:source var-cst))
         (next-ast
           (set-or-bind-variable
            var-cst
            (cleavir-ast:make-if-ast
             (cleavir-ast:make-eq-ast
              ;; The reason we switch to the bound supplied variable
              ;; is so that we can keep the use of the argument
              ;; supplied-p linear for the sake of making the BIR much
              ;; simpler, as arguments in BIR are linear data.
              (if supplied-p-cst
                  (convert-variable supplied-p-cst env system)
                  (cleavir-ast:make-lexical-ast supplied-p :origin origin))
              (convert-constant (make-atom-cst nil origin) env system)
              :origin origin)
             init-ast
             (cleavir-ast:make-lexical-ast var :origin origin)
             :origin origin)
            next-ast
            env system)))
    (if (null supplied-p-cst)
        next-ast
        (set-or-bind-variable supplied-p-cst
                              (cleavir-ast:make-lexical-ast supplied-p :origin origin)
                              next-ast
                              env
                              system))))
