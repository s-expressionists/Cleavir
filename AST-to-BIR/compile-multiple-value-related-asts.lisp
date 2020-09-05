(in-package #:cleavir-ast-to-bir)

(defmethod compile-ast ((ast cleavir-ast:multiple-value-setq-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (eq context :effect))
  (let* ((lhs-asts (cleavir-ast:lhs-asts ast))
         (wvs
           (loop for lhs in lhs-asts
                 for wv = (make-instance 'cleavir-bir:writevar :variable lhs)
                 collect (before inserter wv)))
         (extracts
           (loop for lhs in lhs-asts
                 for var = (find-or-create-variable lhs)
                 for i from 0
                 for extract = (make-instance 'cleavir-bir:extract
                                 :index i :rtype :object)
                 collect (before inserter extract)))
         (rtype (cleavir-bir:make-aggregate (length lhs-asts) :object))
         (val (compile-ast (cleavir-ast:form-ast ast)
                           inserter rtype)))
    (loop for lhs in lhs-asts do (adjoin-variable inserter lhs))
    (loop for wv in wvs
          for extract in extracts
          do (setf (cleavir-bir:inputs extract) (list val)
                   (cleavir-bir:inputs wv) (list extract))))
  (values))

(defun compile-m-v-p1-save (ast inserter)
  (let* ((next (iblock inserter))
         (before
           (make-instance 'cleavir-bir:iblock
             :dynamic-environment (cleavir-bir:dynamic-environment next)))
         (during (make-instance 'cleavir-bir:iblock))
         (alloca (make-instance 'cleavir-bir:alloca :next (list during)))
         (write (make-instance 'cleavir-bir:writetemp))
         (read (make-instance 'cleavir-bir:readtemp :rtype :multiple-values))
         (jump
           (make-instance 'cleavir-bir:jump :inputs nil :next (list next))))
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
          (list (compile-ast (cleavir-ast:first-form-ast ast)
                             inserter :multiple-values)))
    read))

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
  (assert (one-successor-context-p context))
  (let ((arg-asts (cleavir-ast:argument-asts ast)))
    (if (effect-context-p context)
        (compile-sequence-for-effect arg-asts inserter)
        (let* ((rt (cleavir-bir:make-aggregate (length arg-asts) :object))
               (create (make-instance 'cleavir-bir:create :rtype rt))
               (result (figure-values inserter create context)))
          (before inserter create)
          (setf (cleavir-bir:inputs create)
                (compile-arguments arg-asts inserter))
          result))))
