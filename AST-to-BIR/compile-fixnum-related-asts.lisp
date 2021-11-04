(in-package #:cleavir-ast-to-bir)

(defprimop cleavir-primop:fixnum-less ast:fixnum-less-ast
  ast:arg1-ast ast:arg2-ast)
(defprimop cleavir-primop:fixnum-not-greater ast:fixnum-not-greater-ast
  ast:arg1-ast ast:arg2-ast)
(defprimop cleavir-primop:fixnum-equal ast:fixnum-equal-ast
  ast:arg1-ast ast:arg2-ast)

(defmethod compile-test-ast ((ast ast:fixnum-greater-ast) inserter system)
  (with-compiled-asts (rv ((ast:arg1-ast ast) (ast:arg2-ast ast))
                          inserter system)
    (let ((ibs (list (make-iblock inserter) (make-iblock inserter))))
      (terminate inserter 'bir:primop
                 :inputs rv
                 :info '#.(cleavir-primop-info:info
                           'cleavir-primop:fixnum-less)
                 :next (reverse ibs))
      ibs)))

(defmethod compile-test-ast ((ast ast:fixnum-not-less-ast) inserter system)
  (with-compiled-asts (rv ((ast:arg1-ast ast) (ast:arg2-ast ast))
                          inserter system)
    (let ((ibs (list (make-iblock inserter) (make-iblock inserter))))
      (terminate inserter 'bir:primop
                 :inputs rv
                 :info '#.(cleavir-primop-info:info
                           'cleavir-primop:fixnum-not-greater)
                 :next (reverse ibs))
      ibs)))
