(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-set
  (:use #:cl)
  (:shadow #:set #:nunion #:union #:some #:every)
  (:export #:set #:empty-set #:make-set #:arb #:set<= #:set=
           #:nadjoin #:nadjoinf #:nremove #:nremovef
           #:union #:nunion #:nunionf #:nsubtract #:nsubtractf #:difference
           #:presentp #:size #:empty-set-p #:copy-set
           #:doset #:mapset #:set-to-list #:filter #:some #:every))
