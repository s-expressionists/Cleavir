(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-bir-to-cl
  (:use #:cl)
  (:local-nicknames (#:bir #:cleavir-bir)
                    (#:set #:cleavir-set))
  (:export #:translate))
