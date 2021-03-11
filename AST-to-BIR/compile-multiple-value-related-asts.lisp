(in-package #:cleavir-ast-to-bir)

(defmethod compile-ast ((ast cleavir-ast:multiple-value-setq-ast)
                        inserter system)
  (let ((vars (loop for as in (cleavir-ast:lexical-variables ast)
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
  (let* ((during (make-iblock inserter :name '#:mv-prog1-body))
         (de (dynamic-environment inserter))
         (save-out (make-instance 'cleavir-bir:output :rtype :multiple-values))
         (save (make-instance 'cleavir-bir:values-save
                 :inputs (list mv) :outputs (list save-out)
                 :next (list during)))
         (read-out (make-instance 'cleavir-bir:output :rtype :multiple-values))
         (read (make-instance 'cleavir-bir:values-collect
                 :inputs (list save-out) :outputs (list read-out))))
    (setf (cleavir-bir:dynamic-environment during) save)
    (terminate inserter save)
    (begin inserter during)
    (cond ((compile-sequence-for-effect form-asts inserter system)
           (insert inserter read)
           (let ((after (make-iblock inserter
                                     :name '#:mv-prog1-after
                                     :dynamic-environment de)))
             (terminate inserter (make-instance 'cleavir-bir:jump
                                   :inputs () :outputs ()
                                   :next (list after)))
             (begin inserter after))
           read-out)
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
    (let ((form-asts (cleavir-ast:form-asts ast)))
      (cond ((null form-asts)
             (let ((call-out (make-instance 'cleavir-bir:output
                               :rtype :multiple-values)))
               (insert inserter (make-instance 'cleavir-bir:call
                                  :inputs (list (first callee))
                                  :outputs (list call-out)))
               call-out))
            ((null (rest form-asts))
             (with-compiled-arguments (args form-asts inserter system
                                            :multiple-values)
               (let ((mv-call-out (make-instance 'cleavir-bir:output
                                    :rtype :multiple-values)))
                 (insert inserter (make-instance 'cleavir-bir:mv-call
                                    :inputs (list* (first callee)
                                                   (mapcar #'first args))
                                    :outputs (list mv-call-out)))
                 mv-call-out)))
            (t
             (loop with orig-de = (dynamic-environment inserter)
                   for form-ast in (butlast form-asts)
                   for next = (make-iblock inserter :name '#:mv-call-temp)
                   for rv = (compile-ast form-ast inserter system)
                   for mv = (if (eq rv :no-return)
                                (return-from compile-ast :no-return)
                                (adapt inserter rv :multiple-values))
                   for save-out = (make-instance 'cleavir-bir:output
                                    :rtype :multiple-values)
                   for save = (terminate
                               inserter
                               (make-instance 'cleavir-bir:values-save
                                 :inputs mv :outputs (list save-out)
                                 :next (list next)))
                   collect save-out into save-outs
                   do (setf (cleavir-bir:dynamic-environment next) save)
                      (begin inserter next)
                   finally (let* ((last-ast (first (last form-asts)))
                                  (rv (compile-ast last-ast inserter system)))
                             (when (eq rv :no-return)
                               (return-from compile-ast :no-return))
                             (let* ((mv (adapt inserter rv :multiple-values))
                                    (cout (make-instance 'cleavir-bir:output
                                            :rtype :multiple-values))
                                    (c (make-instance
                                           'cleavir-bir:values-collect
                                         :inputs (nconc save-outs mv)
                                         :outputs (list cout)))
                                    (after
                                      (make-iblock inserter
                                                   :dynamic-environment orig-de
                                                   :name '#:mv-call))
                                    (mvcout (make-instance 'cleavir-bir:output
                                              :rtype :multiple-values)))
                               (insert inserter c)
                               (terminate inserter
                                          (make-instance 'cleavir-bir:jump
                                            :inputs () :outputs ()
                                            :next (list after)))
                               (begin inserter after)
                               (insert inserter
                                       (make-instance 'cleavir-bir:mv-call
                                         :inputs (list (first callee) cout)
                                         :outputs (list mvcout)))
                               (return mvcout)))))))))

(defmethod compile-ast ((ast cleavir-ast:values-ast) inserter system)
  (let ((a
          (compile-arguments (cleavir-ast:argument-asts ast) inserter system)))
    (if (eq a :no-return)
        a
        (mapcar #'first a))))
