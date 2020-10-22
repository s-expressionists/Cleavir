(in-package #:cleavir-bir)

(define-condition unused-variable (cleavir-conditions:origin
                                   cleavir-conditions:program-style-warning)
  ((%variable :initarg :variable :reader variable)))
