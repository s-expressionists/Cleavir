(in-package #:cleavir-ast-to-bir)

(defmethod compile-ast ((ast cleavir-ast:primop-ast) inserter system)
  (with-compiled-arguments (args (cleavir-ast:argument-asts ast)
                                 inserter system)
    (let* ((info (cleavir-ast:info ast))
           (out (cleavir-primop-info:out-rtypes info)))
      (cond ((null out)
             (insert inserter
                     (make-instance 'cleavir-bir:nvprimop
                       :info info :inputs (mapcar #'first args))))
            ((integerp out)
             (error "BUG: Test primop in invalid context: ~a" ast))
            (t
             (list
              (insert inserter
                      (make-instance 'cleavir-bir:vprimop
                        :info info :inputs (mapcar #'first args)))))))))

(defmethod compile-test-ast ((ast cleavir-ast:primop-ast) inserter system)
  (with-compiled-arguments (args (cleavir-ast:argument-asts ast)
                                 inserter system)
    (let* ((info (cleavir-ast:info ast))
           (out (cleavir-primop-info:out-rtypes info)))
      (check-type out (integer 0))
      (let ((ibs (loop repeat out collect (make-iblock inserter))))
        (terminate
         inserter
         (make-instance 'cleavir-bir:nvprimop
           :info info :next ibs :inputs args))
        (copy-list ibs)))))

(defmacro defprimop (primop ast &rest readers)
  (let* ((info (cleavir-primop-info:info primop))
         (out (cleavir-primop-info:out-rtypes info))
         (in (cleavir-primop-info:in-rtypes info))
         (kind
           (cond ((null out) 'cleavir-bir:nvprimop)
                 ((integerp out) 'cleavir-bir:tprimop)
                 (t 'cleavir-bir:vprimop)))
         (ca `(,@(loop for reader in readers collect `(,reader ast)))))
    (if (eq kind 'cleavir-bir:tprimop)
        `(defmethod compile-test-ast ((ast ,ast) inserter system)
           (with-compiled-asts (rv ,ca inserter system (,@in))
             (let ((ibs
                     (list ,@(loop repeat out
                                   collect `(make-iblock inserter)))))
               (terminate
                inserter
                (make-instance ',kind :info ',info :next ibs :inputs rv))
               (copy-list ibs))))
        (let ((form
                `(insert inserter
                         (make-instance ',kind :info ',info :inputs rv))))
          `(defmethod compile-ast ((ast ,ast) inserter system)
             (with-compiled-asts (rv ,ca inserter system (,@in))
               ,(if (eq kind 'cleavir-bir:nvprimop) form `(list ,form))))))))

(defprimop symbol-value cleavir-ast:symbol-value-ast
  cleavir-ast:symbol-ast)
(defprimop (setf symbol-value) cleavir-ast:set-symbol-value-ast
  cleavir-ast:symbol-ast cleavir-ast:value-ast)
(defprimop fdefinition cleavir-ast:fdefinition-ast
  cleavir-ast:name-ast)
