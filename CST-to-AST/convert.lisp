(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT is responsible for converting a concrete syntax tree to an
;;; abstract syntax tree.

(defmethod convert (cst environment system)
  (let ((form (cst:raw cst)))
    (cond ((and (not (consp form)) (not (symbolp form)))
           (convert-constant cst environment system))
          ((symbolp form)
           (convert-variable cst environment system))
          ((symbolp (car form))
           ;; Even if we are in COMPILE-TIME-TOO mode, at this point, we
           ;; do not know whether to evaluate the form at compile time,
           ;; simply because it might be a special form that is handled
           ;; specially.  So we must wait until we have more
           ;; information.
           (let ((info (function-info system environment (cst:first cst))))
             (convert-cst cst info environment system)))
          (t
           ;; The form must be a compound form where the CAR is a lambda
           ;; expression.  Evaluating such a form might have some
           ;; compile-time side effects, so we must check whether we are
           ;; in COMPILE-TIME-TOO mode, in which case we must evaluate
           ;; the form as well.
           (when (and *current-form-is-top-level-p* *compile-time-too*)
             (cst-eval-for-effect cst environment system))
           (convert-lambda-call cst environment system)))))

(defmethod convert :around (cst environment system)
  (restart-case
      ;; We bind these only here so that if a restart is invoked,
      ;; the new CONVERT call will get the right values
      ;; (i.e., the ones outside our binding)
      (let ((*current-form-is-top-level-p* *subforms-are-top-level-p*)
            (*subforms-are-top-level-p* nil)
            ;; gives all generated ASTs the appropriate policy.
            (ast:*policy* (env:environment-policy system environment)))
        (call-next-method))
    (continue ()
      :report "Replace with call to ERROR."
      (convert (cst:cst-from-expression
                `(error 'run-time-program-error
                        :expr ',(cst:raw cst)
                        :origin ',(cst:source cst))
                :source (cst:source cst))
               environment system))
    (substitute-cst (cst)
      :report "Compile the given CST in place of the problematic one."
      (convert cst environment system))))
