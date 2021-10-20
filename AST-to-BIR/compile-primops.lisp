(in-package #:cleavir-ast-to-bir)

(defmethod compile-ast ((ast cleavir-ast:primop-ast) inserter system)
  (with-compiled-arguments (args (cleavir-ast:argument-asts ast)
                                 inserter system)
    (let* ((info (cleavir-ast:info ast))
           (out (cleavir-primop-info:out-kind info)))
      (when (integerp out)
        (error "BUG: Test primop in invalid context: ~a" ast))
      (let ((outputs (ecase out
                       ((:value) (list (make-instance 'cleavir-bir:output)))
                       ((:effect) nil))))
        (insert inserter
                (make-instance 'cleavir-bir:vprimop
                  :info info :inputs args :outputs outputs))
        (copy-list outputs)))))

(defmethod compile-test-ast ((ast cleavir-ast:primop-ast) inserter system)
  (with-compiled-arguments (args (cleavir-ast:argument-asts ast)
                                 inserter system)
    (let* ((info (cleavir-ast:info ast))
           (out (cleavir-primop-info:out-kind info)))
      (check-type out (integer 0))
      (let ((ibs (loop repeat out collect (make-iblock inserter))))
        (terminate
         inserter
         (make-instance 'cleavir-bir:tprimop
           :info info :next ibs :inputs args))
        (copy-list ibs)))))

(defmacro defprimop (primop ast &rest readers)
  (let* ((info (cleavir-primop-info:info primop))
         (out (cleavir-primop-info:out-kind info))
         (ca `(,@(loop for reader in readers collect `(,reader ast)))))
    (if (integerp out)
        `(defmethod compile-test-asts ((ast ,ast) inserter system)
           (with-compiled-asts (rv ,ca inserter system)
             (let ((ibs
                     (list ,@(loop repeat out
                                   collect `(make-iblock inserter)))))
               (terminate
                inserter
                (make-instance 'cleavir-bir:tprimop
                  :info ',info :next ibs :inputs rv))
               (copy-list ibs))))
        `(defmethod compile-ast ((ast ,ast) inserter system)
           (with-compiled-asts (rv ,ca inserter system)
             (let ((outs ,(ecase out
                            ((:value)
                             '(list (make-instance 'cleavir-bir:output)))
                            ((:effect) nil))))
               (insert inserter
                       (make-instance 'cleavir-bir:vprimop
                         :info ',info :inputs rv :outputs outs))
               (copy-list outs)))))))

(defprimop symbol-value cleavir-ast:symbol-value-ast
  cleavir-ast:symbol-ast)
(defprimop (setf symbol-value) cleavir-ast:set-symbol-value-ast
  cleavir-ast:symbol-ast cleavir-ast:value-ast)

;;; Make sure the output of an fdefinition gets the attributes.
(defmethod compile-ast ((ast cleavir-ast:fdefinition-ast) inserter system)
  (with-compiled-asts (rv ((cleavir-ast:name-ast ast)) inserter system)
    (let ((out (make-instance 'cleavir-bir:output
                 :attributes (cleavir-ast:attributes ast))))
      (insert inserter
              (make-instance 'cleavir-bir:vprimop
                :info (cleavir-primop-info:info 'fdefinition)
                :inputs rv :outputs (list out)))
      (list out))))
