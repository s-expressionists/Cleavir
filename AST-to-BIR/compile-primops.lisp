(in-package #:cleavir-ast-to-bir)

(defmacro defprimop (primop ast &rest readers)
  (let* ((info (cleavir-bir:primop-info primop))
         (nv-p (null (cleavir-bir:out-rtypes info)))
         (cname
           (if nv-p 'cleavir-bir:nvprimop 'cleavir-bir:vprimop))
         (form
           `(make-instance ',cname :info ',info :inputs rv)))
    `(defmethod compile-ast ((ast ,ast) inserter)
       (let ((rv (compile-arguments
                  (list ,@(loop for reader in readers
                                collect `(,reader ast)))
                  inserter)))
         (when (eq rv :no-return) (return-from compile-ast rv))
         ,(if nv-p
              `(insert inserter ,form)
              `(list (insert inserter ,form)))))))

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
