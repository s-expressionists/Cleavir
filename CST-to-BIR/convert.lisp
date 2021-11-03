(in-package #:cleavir-cst-to-bir)

(defmethod convert (cst inserter environment system)
  (let ((form (cst:raw cst)))
    (cond ((and (not (consp form)) (not (symbolp form)))
           (convert-constant cst inserter))
          ((symbolp form)
           (convert-variable cst inserter environment system))
          ((symbolp (car form))
           (let ((info (function-info system environment (cst:first cst))))
             (convert-cst cst info inserter environment system)))
          (t
           #+(or)
           (when (and *current-form-is-top-level-p* *compile-time-too*)
             (cst-eval-for-effect cst environment system))
           (convert-lambda-call cst inserter environment system)))))

(defmethod convert :around (cst inserter environment system)
  (restart-case
      (let (#+(or)(*current-form-is-top-level-p* *subforms-are-top-level-p*)
            #+(or)(*subforms-are-top-level-p* nil)
            (bir:*policy* (env:environment-policy environment))
            (bir:*origin* (cst:source cst)))
        (call-next-method))
    (continue ()
      :report "Replace with call to ERROR."
      (convert (cst:cst-from-expression
                `(error 'run-time-program-error
                        :expr ',(cst:raw cst)
                        :origin ',(cst:source cst))
                :source (cst:source cst))
               inserter environment system))
    (substitute-cst (cst)
      :report "Compile the given CST in place of the problematic one."
      (convert cst inserter environment system))))
