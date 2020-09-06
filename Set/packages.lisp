(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-set
  (:use #:cl)
  (:shadow #:set)
  (:export #:set #:empty-set #:make-set #:arb #:set=
           #:nset-adjoin #:nset-adjoinf #:nset-remove #:nset-removef
           #:nset-union #:nset-unionf
           #:presentp #:set-size #:empty-set-p #:copy-set
           #:doset #:mapset #:set-to-list #:set-filter #:set-every))
