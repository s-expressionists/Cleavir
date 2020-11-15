(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-bir-transformations
  (:use #:cl)
  (:export #:module-eliminate-catches #:eliminate-catches)
  (:export #:process-captured-variables)
  (:export #:module-optimize-variables)
  (:export #:simple-unwinding-p)
  (:export #:find-module-local-calls)
  (:export #:dynamic-extent-analyze-closures))
