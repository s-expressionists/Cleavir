(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-bir-transformations
  (:use #:cl)
  (:export #:process-captured-variables)
  (:export #:delete-temporary-variables))
