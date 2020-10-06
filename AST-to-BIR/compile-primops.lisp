(in-package #:cleavir-ast-to-bir)

(defmacro defprimop (primop ast &rest readers)
  (let* ((info (cleavir-bir:primop-info primop))
         (out (cleavir-bir:out-rtypes info))
         (kind
           (cond ((null out) 'cleavir-bir:nvprimop)
                 ((integerp out) 'cleavir-bir:tprimop)
                 (t 'cleavir-bir:vprimop)))
         (ca
           `(compile-arguments (list ,@(loop for reader in readers
                                             collect `(,reader ast)))
                               inserter system)))
    (if (eq kind 'cleavir-bir:tprimop)
        `(defmethod compile-test-ast ((ast ,ast) inserter system)
           (let ((rv ,ca))
             (when (eq rv :no-return) (return-from compile-test-ast rv))
             (let ((ibs
                     (list ,@(loop repeat out
                                   collect `(make-iblock inserter)))))
               (terminate
                inserter
                (make-instance ',kind :info ',info :inputs rv :next ibs))
               (copy-list ibs))))
        (let ((form
                `(insert inserter
                         (make-instance ',kind :info ',info :inputs rv))))
          `(defmethod compile-ast ((ast ,ast) inserter system)
             (let ((rv ,ca))
               (when (eq rv :no-return) (return-from compile-ast rv))
               ,(if (eq kind 'cleavir-bir:nvprimop) form `(list ,form))))))))

(defprimop cleavir-primop:car cleavir-ast:car-ast cleavir-ast:cons-ast)
(defprimop cleavir-primop:cdr cleavir-ast:cdr-ast cleavir-ast:cons-ast)
(defprimop cleavir-primop:rplaca cleavir-ast:rplaca-ast
  cleavir-ast:cons-ast cleavir-ast:object-ast)
(defprimop cleavir-primop:rplacd cleavir-ast:rplacd-ast
  cleavir-ast:cons-ast cleavir-ast:object-ast)
(defprimop symbol-value cleavir-ast:symbol-value-ast
  cleavir-ast:symbol-ast)
(defprimop (setf symbol-value) cleavir-ast:set-symbol-value-ast
  cleavir-ast:symbol-ast cleavir-ast:value-ast)
(defprimop fdefinition cleavir-ast:fdefinition-ast
  cleavir-ast:name-ast)
