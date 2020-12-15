(in-package #:cleavir-ast-to-bir)

(defmethod compile-ast ((ast cleavir-ast:multiple-value-setq-ast)
                        inserter system)
  (let ((vars (loop for as in (cleavir-ast:lhs-asts ast)
                    collect (find-variable as))))
    (with-compiled-ast (vals (cleavir-ast:form-ast ast) inserter system
                             (make-list (length vars)
                                        :initial-element :object))
      (loop for var in vars
            for val in vals
            for wv = (make-instance 'cleavir-bir:writevar
                       :inputs (list val) :outputs (list var))
            do (insert inserter wv))
      ())))

(defun compile-m-v-p1-save (inserter system mv form-asts)
  ;; Note that there are further situations we don't need to save.
  ;; If the user of the m-v-p1 only needs fixed values, those could just be
  ;; extracted early and no saving done. We don't have that information at this
  ;; moment, so an optimization pass could rewrite it. Alternately AST-to-BIR
  ;; could be rewritten to account for this kind of context.
  (let* ((during (make-iblock inserter))
         (de (dynamic-environment inserter))
         (save (make-instance 'cleavir-bir:values-save
                 :inputs (list mv) :next (list during)))
         (read (make-instance 'cleavir-bir:values-collect
                 :inputs (list save))))
    (setf (cleavir-bir:dynamic-environment during) save)
    (terminate inserter save)
    (begin inserter during)
    (cond ((compile-sequence-for-effect form-asts inserter system)
           (insert inserter read)
           (let ((after (make-iblock inserter :dynamic-environment de)))
             (terminate inserter (make-instance 'cleavir-bir:jump
                                   :inputs () :outputs ()
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
  (with-compiled-ast (callee (cleavir-ast:function-form-ast ast)
                      inserter system)
    (with-compiled-arguments (args (cleavir-ast:form-asts ast) inserter system
                                   :multiple-values)
      (insert inserter (make-instance 'cleavir-bir:mv-call
                         :inputs (list* (first callee)
                                        (mapcar #'first args)))))))

(defmethod compile-ast ((ast cleavir-ast:values-ast) inserter system)
  (let ((a
          (compile-arguments (cleavir-ast:argument-asts ast) inserter system)))
    (if (eq a :no-return)
        a
        (mapcar #'first a))))

(defun compile-m-v-extract (inserter system lhss mv form-asts)
  (let* ((during (make-iblock inserter))
         (de (dynamic-environment inserter))
         (alloca (make-instance 'cleavir-bir:alloca
                   :rtype :multiple-values :next (list during)))
         (write (make-instance 'cleavir-bir:writetemp
                  :alloca alloca :inputs (list mv)))
         (read1 (make-instance 'cleavir-bir:readtemp
                  :alloca alloca :rtype :multiple-values))
         (rtypes (make-list (length lhss) :initial-element :object))
         (read2 (make-instance 'cleavir-bir:readtemp
                  :alloca alloca :rtype :multiple-values)))
    (setf (cleavir-bir:dynamic-environment during) alloca)
    (terminate inserter alloca)
    (begin inserter during)
    (insert inserter write)
    (insert inserter read1)
    ;; Set the variables.
    (let ((fv (adapt inserter read1 rtypes)))
      (loop for lhs in lhss
            for var = (find-variable lhs)
            for f in fv
            for wv = (make-instance 'cleavir-bir:writevar
                       :inputs (list f) :outputs (list var))
            do (insert inserter wv)))
    ;; Compile the body
    (cond ((compile-sequence-for-effect form-asts inserter system)
           (insert inserter read2)
           (let ((after (make-iblock inserter :dynamic-environment de)))
             (terminate inserter (make-instance 'cleavir-bir:jump
                                   :inputs () :outputs ()
                                   :next (list after)))
             (begin inserter after))
           read2)
          (t
           ;; the forms did not return.
           ;; This makes our saving pointless, so hypothetically we could go back
           ;; and change that stuff.
           :no-return))))

(defmethod compile-ast ((ast cleavir-ast:multiple-value-extract-ast)
                        inserter system)
  (let ((rv (compile-ast (cleavir-ast:first-form-ast ast) inserter system)))
    (cond ((eq rv :no-return) rv)
          ((listp rv)
           ;; Write the variables.
           (let ((vars (mapcar #'find-variable (cleavir-ast:lhs-asts ast)))
                 (nvals (length rv)))
             (loop for var in vars
                   ;; If there are more variables than values, compensate
                   for r = (or (pop rv)
                               (insert inserter (cleavir-bir:make-constant-reference
                                                 (cleavir-bir:constant-in-module 'nil *current-module*))))
                   for wv = (make-instance 'cleavir-bir:writevar
                              :inputs (list r) :outputs (list var))
                   do (insert inserter wv))
             ;; A bunch of values are returned, so we don't need to save.
             (if (compile-sequence-for-effect (cleavir-ast:form-asts ast)
                                              inserter system)
                 ;; Return readvars plus any extra values, if there were
                 ;; more values than variables.
                 (append
                  (loop repeat nvals ; cut out early if there are more vars
                        for var in vars
                        for rvar = (make-instance 'cleavir-bir:readvar
                                     :inputs (list var))
                        do (insert inserter rvar)
                        collect rvar)
                  rv)
                 :no-return)))
          (t
           ;; Multiple values were returned. Save.
           (compile-m-v-extract inserter system
                                (cleavir-ast:lhs-asts ast)
                                rv (cleavir-ast:form-asts ast))))))
