(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-ast-to-bir
  (:use #:cl)
  (:shadow #:function)
  (:local-nicknames (#:set #:cleavir-set)
                    (#:ast #:cleavir-ast)
                    (#:bir #:cleavir-bir)
                    (#:build #:cleavir-bir-builder)
                    (#:ctype #:cleavir-ctype))
  (:export #:compile-toplevel #:compile-into-module #:compile-function
           #:compile-ast #:compile-test-ast #:compile-arguments
           #:compile-sequence-for-effect)
  (:export #:with-compiled-ast #:with-compiled-asts #:with-compiled-arguments)
  (:export #:defprimop))
