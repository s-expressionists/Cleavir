(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-bir-transformations
  (:use #:cl)
  (:export #:module-eliminate-catches #:eliminate-catches)
  (:export #:module-optimize-variables)
  (:export #:simple-unwinding-p)
  (:export #:find-module-local-calls)
  (:export #:determine-function-environments)
  (:export #:determine-closure-extents)
  (:export #:determine-variable-extents))
