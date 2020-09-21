(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-ast-to-bir2
  (:use #:cl)
  (:shadow #:function)
  (:export #:compile-toplevel #:compile-function
           #:compile-ast #:compile-test-ast #:compile-arguments
           #:compile-sequence-for-effect)
  (:export #:inserter #:make-iblock #:begin #:proceed #:insert #:terminate
           #:adapt #:function #:dynamic-environment))
