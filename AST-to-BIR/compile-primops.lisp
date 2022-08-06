(in-package #:cleavir-ast-to-bir)

(defmethod compile-ast ((ast ast:primop-ast) inserter system)
  (with-compiled-arguments (args (ast:argument-asts ast) inserter system)
    (let* ((info (ast:info ast))
           (out (cleavir-primop-info:out-kind info)))
      (let ((outputs (ecase out
                       ((:value) (list (make-instance 'bir:output)))
                       ((:effect) nil))))
        (insert inserter 'bir:primop
                :info info :inputs args :outputs outputs)
        (copy-list outputs)))))

(defmethod compile-test-ast ((ast ast:primop-ast) inserter system)
  (with-compiled-arguments (args (ast:argument-asts ast) inserter system)
    (let* ((info (ast:info ast)))
      (let* ((ibs (loop repeat 2 collect (make-iblock inserter)))
             (p-out (make-instance 'bir:output))
             (p (make-instance 'bir:primop
                  :info info :inputs args :outputs (list p-out))))
        (insert inserter p)
        (terminate inserter 'bir:ifi
                   :inputs (list p-out) :next ibs)
        (copy-list ibs)))))

(defmacro defprimop (primop ast &rest readers)
  (let* ((info (cleavir-primop-info:info primop))
         (out (cleavir-primop-info:out-kind info))
         (ca `(,@(loop for reader in readers collect `(,reader ast)))))
    (ecase out
      ((2)
       `(defmethod compile-test-ast ((ast ,ast) inserter system)
          (with-compiled-asts (args ,ca inserter system)
            (let* ((ibs
                     (list ,@(loop repeat out
                                   collect `(make-iblock inserter))))
                   (p-out (make-instance 'bir:output))
                   (p (make-instance 'bir:primop
                        :info ',info :inputs args :outputs (list p-out))))
              (insert inserter p)
              (terminate inserter 'bir:ifi
                         :inputs (list p-out) :next ibs)
              (copy-list ibs)))))
      ((:value :effect)
       `(defmethod compile-ast ((ast ,ast) inserter system)
          (with-compiled-asts (args ,ca inserter system)
            (let ((outs ,(ecase out
                           ((:value)
                            '(list (make-instance 'bir:output)))
                           ((:effect) nil))))
              (insert inserter 'bir:primop
                      :info ',info :inputs args :outputs outs)
              (copy-list outs))))))))

(defprimop symbol-value ast:symbol-value-ast
  ast:symbol-ast)
(defprimop (setf symbol-value) ast:set-symbol-value-ast
  ast:symbol-ast ast:value-ast)

;;; Make sure the output of an fdefinition gets the attributes.
(defmethod compile-ast ((ast ast:fdefinition-ast) inserter system)
  (with-compiled-asts (rv ((ast:name-ast ast)) inserter system)
    (let ((out (make-instance 'bir:output
                 :attributes (ast:attributes ast))))
      (insert inserter 'bir:primop
              :info (cleavir-primop-info:info 'fdefinition)
              :inputs rv :outputs (list out))
      (list out))))
