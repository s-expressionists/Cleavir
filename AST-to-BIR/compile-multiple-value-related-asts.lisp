(in-package #:cleavir-ast-to-bir)

(defmethod compile-ast ((ast cleavir-ast:multiple-value-setq-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (effect-context-p context))
  (let* ((lhs-asts (cleavir-ast:lhs-asts ast))
         (wvs
           (loop for lhs in lhs-asts
                 for wv = (make-instance 'cleavir-bir:writevar
                            :outputs (list lhs))
                 collect (before inserter wv)))
         (vals (compile-ast (cleavir-ast:form-ast ast)
                            inserter (make-list (length lhs-asts)
                                                :initial-element :object))))
    (loop for lhs in lhs-asts do (adjoin-variable inserter lhs))
    (loop for wv in wvs for val in vals
          do (setf (cleavir-bir:inputs wv) (list val))))
  (values))

(defun compile-m-v-p1-save (ast inserter)
  (let* ((next (iblock inserter))
         (before (make-iblock inserter))
         (during (make-iblock inserter))
         (alloca (make-instance 'cleavir-bir:alloca
                   :rtype :multiple-values :next (list during)))
         (write (make-instance 'cleavir-bir:writetemp))
         (read (make-instance 'cleavir-bir:readtemp :rtype :multiple-values))
         (jump (make-instance 'cleavir-bir:jump
                 :unwindp t :inputs nil :outputs nil :next (list next))))
    (setf (cleavir-bir:dynamic-environment during) alloca)
    (finalize inserter)
    (reset inserter during)
    (terminate inserter jump)
    (before inserter read)
    (compile-sequence-for-effect (cleavir-ast:form-asts ast) inserter)
    (before inserter write)
    (finalize inserter)
    (reset inserter before)
    (terminate inserter alloca)
    (setf (cleavir-bir:inputs write)
          (compile-ast (cleavir-ast:first-form-ast ast)
                       inserter :multiple-values))
    (list read)))

(defmethod compile-ast ((ast cleavir-ast:multiple-value-prog1-ast)
                        inserter context)
  (check-type inserter inserter)
  (cond ((eq context :multiple-values)
         (compile-m-v-p1-save ast inserter))
        ;; don't actually have to do saving or loading
        (t
         (compile-sequence-for-effect (cleavir-ast:form-asts ast) inserter)
         (compile-ast (cleavir-ast:first-form-ast ast) inserter context))))

(defmethod compile-ast ((ast cleavir-ast:values-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (context-p context))
  (let ((arg-asts (cleavir-ast:argument-asts ast)))
    (case context
      (:effect (compile-sequence-for-effect arg-asts inserter))
      (:multiple-values
       (let ((ftm (make-instance 'cleavir-bir:fixed-to-multiple)))
         (before inserter ftm)
         (setf (cleavir-bir:inputs ftm)
               (compile-arguments arg-asts inserter))
         (list ftm)))
      (t (figure-n-values inserter
                          (compile-arguments arg-asts inserter)
                          context)))))
