(in-package #:cleavir-ast-to-bir)

(defmethod compile-ast (client (ast ast:primop-ast) inserter)
  (with-compiled-arguments (args client (ast:argument-asts ast) inserter)
    (let* ((info (ast:info ast))
           (out (cleavir-primop-info:out-kind info)))
      (let ((outputs (ecase out
                       ((:value) (list (make-instance 'bir:output)))
                       ((:effect) nil))))
        (insert inserter 'bir:primop
                :info info :inputs args :outputs outputs)
        (copy-list outputs)))))

(defmethod compile-test-ast (client (ast ast:primop-ast) inserter)
  (with-compiled-arguments (args client (ast:argument-asts ast) inserter)
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
       `(defmethod compile-test-ast (client (ast ,ast) inserter)
          (with-compiled-asts (args client ,ca inserter)
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
       `(defmethod compile-ast (client (ast ,ast) inserter)
          (with-compiled-asts (args client ,ca inserter)
            (let ((outs ,(ecase out
                           ((:value)
                            '(list (make-instance 'bir:output)))
                           ((:effect) nil))))
              (insert inserter 'bir:primop
                      :info ',info :inputs args :outputs outs)
              (copy-list outs))))))))
