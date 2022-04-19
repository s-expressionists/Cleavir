(cl:in-package #:common-lisp-user)

(defpackage :cleavir-stealth-mixins
  (:use #:common-lisp)
  (:export
   #:class-stealth-mixins
   #:add-stealth-mixin
   #:victim-class
   #:define-stealth-mixin))
