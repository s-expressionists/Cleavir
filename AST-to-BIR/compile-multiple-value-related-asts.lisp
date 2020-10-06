(in-package #:cleavir-ast-to-bir)

(defmethod compile-ast ((ast cleavir-ast:multiple-value-setq-ast)
                        inserter system)
  (let ((rv (compile-ast (cleavir-ast:form-ast ast) inserter system)))
    (when (eq rv :no-return) (return-from compile-ast rv))
    (let* ((vars (loop for as in (cleavir-ast:lhs-asts ast)
                       collect (find-variable as)))
           (vals (adapt inserter rv (make-list (length vars)
                                               :initial-element :object))))
      (loop for var in vars
            for val in vals
            for wv = (make-instance 'cleavir-bir:writevar
                       :inputs (list val) :outputs (list var))
            do (insert inserter wv))))
  ())

(defun compile-m-v-p1-save (inserter system mv form-asts)
  ;; Note that there are further situations we don't need to save.
  ;; If the user of the m-v-p1 only needs fixed values, those could just be
  ;; extracted early and no saving done. We don't have that information at this
  ;; moment, so an optimization pass could rewrite it. Alternately AST-to-BIR
  ;; could be rewritten to account for this kind of context.
  (let* ((during (make-iblock inserter))
         (de (dynamic-environment inserter))
         (alloca (make-instance 'cleavir-bir:alloca
                   :rtype :multiple-values :next (list during)))
         (write (make-instance 'cleavir-bir:writetemp :inputs (list mv)))
         (read (make-instance 'cleavir-bir:readtemp :rtype :multiple-values)))
    (setf (cleavir-bir:dynamic-environment during) alloca)
    (terminate inserter alloca)
    (begin inserter during)
    (insert inserter write)
    (cond ((compile-sequence-for-effect form-asts inserter system)
           (insert inserter read)
           (let ((after (make-iblock inserter :dynamic-environment de)))
             (terminate inserter (make-instance 'cleavir-bir:jump
                                   :inputs () :outputs () :unwindp t
                                   :next (list after)))
             (begin inserter after))
           read)
          (t
           ;; the forms did not return.
           ;; This makes our saving pointless, so hypothetically we could go back
           ;; and change that stuff.
           :no-return))))

(defmethod compile-ast ((ast cleavir-ast:multiple-value-prog1-ast)
                        inserter system)
  (let ((rv (compile-ast (cleavir-ast:first-form-ast ast) inserter system)))
    (cond ((eq rv :no-return) rv)
          ((listp rv)
           ;; A bunch of values are returned, so we don't need to save.
           (if (compile-sequence-for-effect (cleavir-ast:form-asts ast)
                                            inserter system)
               rv
               :no-return))
          (t
           ;; Multiple values were returned. Save.
           (compile-m-v-p1-save inserter system
                                rv (cleavir-ast:form-asts ast))))))

;;; Semantics of mv-call could be rethought. For example if all the argument
;;; forms produce a fixed number of values we could just make a call?
(defmethod compile-ast ((ast cleavir-ast:multiple-value-call-ast)
                        inserter system)
  (let ((callee (compile-ast (cleavir-ast:function-form-ast ast)
                             inserter system)))
    (when (eq callee :no-return) (return-from compile-ast :no-return))
    (let ((callee2 (first (adapt inserter callee '(:object))))
          (args (loop for a in (cleavir-ast:form-asts ast)
                      for rv = (compile-ast a inserter system)
                      if (eq rv :no-return)
                        do (return-from compile-ast :no-return)
                      else append (adapt inserter rv :multiple-values))))
      (insert inserter (make-instance 'cleavir-bir:mv-call
                         :inputs (list* callee2 args))))))

(defmethod compile-ast ((ast cleavir-ast:values-ast) inserter system)
  (compile-arguments (cleavir-ast:argument-asts ast) inserter system))
