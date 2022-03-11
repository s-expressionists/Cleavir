(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-abstract-interpreter
  (:use #:cl)
  (:local-nicknames (#:bir #:cleavir-bir)
                    (#:set #:cleavir-set)
                    (#:ctype #:cleavir-ctype)
                    (#:attributes #:cleavir-attributes))
  (:shadow #:type)
  (:export))
