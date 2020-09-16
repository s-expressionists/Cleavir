(in-package #:cleavir-ast-to-bir)

(defun compile-m-v-setq (lhs-vars form-ast inserter)
  (let* ((wvs
           (loop for lhs in lhs-vars
                 for wv = (make-instance 'cleavir-bir:writevar
                            :outputs (list lhs))
                 collect (before inserter wv)))
         (vals (compile-ast form-ast inserter
                            (make-list (length lhs-vars)
                                       :initial-element :object))))
    (loop for lhs in lhs-vars do (adjoin-variable inserter lhs))
    (loop for wv in wvs for val in vals
          do (setf (cleavir-bir:inputs wv) (list val))))
  (values))

(defmethod compile-ast ((ast cleavir-ast:multiple-value-setq-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (effect-context-p context))
  (compile-m-v-setq (loop for ast in (cleavir-ast:lhs-asts ast)
                         collect (find-or-create-variable
                                  ast (function inserter)))
                    (cleavir-ast:form-ast ast)
                    inserter)
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

(defmethod compile-ast ((ast cleavir-ast:multiple-value-extract-ast)
                        inserter context)
  (let ((lhs-vars (loop for ast in (cleavir-ast:lhs-asts ast)
                        collect (find-or-create-variable
                                 ast (function inserter))))
        (first-form-ast (cleavir-ast:first-form-ast ast))
        (form-asts (cleavir-ast:form-asts ast)))
    (case context
      ((:effect)
       (compile-sequence-for-effect form-asts inserter)
       (compile-m-v-setq lhs-vars first-form-ast inserter))
      ((:multiple-values)
       ;; Here, we actually have to save the values.
       ;; This is kind of messy. Mostly because we need the values twice, which
       ;; doesn't go great with the whole linear-datum business.
       ;; We compensate by reading from the temporary storage twice.
       ;; On most implementations the first read probably isn't necessary, but
       ;; this is hopefully removable by a general pass to remove storage.
       (let* ((next (iblock inserter))
              (before (make-iblock inserter))
              (during (make-iblock inserter))
              (alloca (make-instance 'cleavir-bir:alloca
                        :rtype :multiple-values :next (list during)))
              (write (make-instance 'cleavir-bir:writetemp))
              (mtf-rtype
                (make-list (length lhs-vars) :initial-element :object))
              (read1 (make-instance 'cleavir-bir:readtemp
                      :rtype :multiple-values))
              (read2 (make-instance 'cleavir-bir:readtemp
                      :rtype :multiple-values))
              (jump (make-instance 'cleavir-bir:jump
                      :unwindp t :inputs nil :outputs nil :next (list next))))
         (setf (cleavir-bir:dynamic-environment during) alloca)
         (finalize inserter)
         (reset inserter during)
         (terminate inserter jump)
         (before inserter read2)
         (compile-sequence-for-effect form-asts inserter)
         (multiple-value-bind (mtf outs)
             (cleavir-bir:make-multiple-to-fixed read1 mtf-rtype)
           (loop for var in lhs-vars
                 for out in outs
                 do (before inserter (make-instance 'cleavir-bir:writevar
                                       :inputs (list out) :outputs (list var))))
           (before inserter mtf))
         (before inserter read1)
         (before inserter write)
         (finalize inserter)
         (reset inserter before)
         (terminate inserter alloca)
         (setf (cleavir-bir:inputs write)
               (compile-ast first-form-ast inserter :multiple-values))
         (list read2)))
      (t
       (let ((reads (loop for var in lhs-vars
                          collect (before inserter
                                          (make-instance 'cleavir-bir:readvar
                                            :inputs (list var))))))
         (compile-sequence-for-effect form-asts inserter)
         (compile-m-v-setq lhs-vars first-form-ast inserter)
         (figure-n-values inserter reads context))))))

(defmethod compile-ast ((ast cleavir-ast:values-ast) inserter context)
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
