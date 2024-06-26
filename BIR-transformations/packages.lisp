(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-bir-transformations
  (:use #:cl)
  (:local-nicknames (#:bir #:cleavir-bir)
                    (#:set #:cleavir-set)
                    (#:ctype #:cleavir-ctype)
                    (#:attributes #:cleavir-attributes))
  (:export #:module-eliminate-come-froms #:eliminate-come-froms)
  (:export #:module-optimize-variables #:function-optimize-variables)
  (:export #:simple-unwinding-p #:simple-dynenv-p)
  (:export #:find-module-local-calls
           #:interpolate-module-calls #:maybe-interpolate)
  (:export #:determine-function-environments)
  (:export #:determine-closure-extents)
  (:export #:determine-variable-extents)
  (:export #:meta-evaluate-module
           #:generate-type-check-function
           #:transform-call #:fold-call #:derive-return-type
           #:flushable-call-p)
  (:export #:module-generate-type-checks))
