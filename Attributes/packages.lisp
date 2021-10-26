(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-attributes
  (:use #:common-lisp)
  (:export #:attributes #:attributes-designator #:default-attributes
           #:transforms #:derivers
           #:meet-attributes #:join-attributes)
  (:export #:make-flags #:has-flag-p))
