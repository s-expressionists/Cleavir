(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-conditions
  (:use #:common-lisp)
  (:shadow #:program-error)
  (:export #:program-condition
           #:program-error #:program-warning #:program-style-warning)
  (:export #:origin)
  (:export #:program-note #:note #:muffle-note))
