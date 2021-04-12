(in-package #:cleavir-ast-to-bir)

(defprimop cleavir-primop:fixnum-less cleavir-ast:fixnum-less-ast
  cleavir-ast:arg1-ast cleavir-ast:arg2-ast)
(defprimop cleavir-primop:fixnum-not-greater cleavir-ast:fixnum-not-greater-ast
  cleavir-ast:arg1-ast cleavir-ast:arg2-ast)
(defprimop cleavir-primop:fixnum-equal cleavir-ast:fixnum-equal-ast
  cleavir-ast:arg1-ast cleavir-ast:arg2-ast)

(defmethod compile-test-ast ((ast cleavir-ast:fixnum-greater-ast)
                             inserter system)
  (with-compiled-asts (rv ((cleavir-ast:arg1-ast ast)
                           (cleavir-ast:arg2-ast ast))
                          inserter system)
    (let ((ibs (list (make-iblock inserter) (make-iblock inserter))))
      (terminate inserter (make-instance 'cleavir-bir:tprimop
                            :inputs (mapcar #'first rv)
                            :info '#.(cleavir-primop-info:info
                                      'cleavir-primop:fixnum-less)
                            :next (reverse ibs)))
      ibs)))

(defmethod compile-test-ast ((ast cleavir-ast:fixnum-not-less-ast)
                             inserter system)
  (with-compiled-asts (rv ((cleavir-ast:arg1-ast ast)
                           (cleavir-ast:arg2-ast ast))
                          inserter system)
    (let ((ibs (list (make-iblock inserter) (make-iblock inserter))))
      (terminate inserter (make-instance 'cleavir-bir:tprimop
                            :inputs (mapcar #'first rv)
                            :info '#.(cleavir-primop-info:info
                                      'cleavir-primop:fixnum-not-greater)
                            :next (reverse ibs)))
      ibs)))
