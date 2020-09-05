(in-package #:cleavir-ast-to-bir)

(macrolet ((defprimop (primop ast &rest readers)
             (let* ((info (cleavir-bir:primop-info primop))
                    (nv-p (null (cleavir-bir:rtype info)))
                    (cname
                      (if nv-p 'cleavir-bir:nvprimop 'cleavir-bir:vprimop)))
               `(defmethod compile-ast ((ast ,ast) inserter context)
                  (check-type inserter inserter)
                  (assert
                   ,(if nv-p
                        '(effect-context-p context)
                        '(one-successor-context-p context)))
                  (let ((p (make-instance ',cname :info ',info)))
                    (prog1 ,(if nv-p
                                '(values)
                                '(figure-values inserter p context))
                      (before inserter p)
                      (setf (cleavir-bir:inputs p)
                            (compile-arguments
                             (list ,@(loop for reader in readers
                                           collect `(,reader ast)))
                             inserter))))))))
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
    cleavir-ast:name-ast))
