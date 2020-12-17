(in-package #:cleavir-bir)

(define-condition unused-variable (cleavir-conditions:origin
                                   cleavir-conditions:program-style-warning)
  ((%variable :initarg :variable :reader variable)))

(define-condition type-conflict (cleavir-conditions:origin
                                 cleavir-conditions:program-warning)
  ((%derived-type :initarg :derived-type :reader derived-type)
   (%asserted-type :initarg :asserted-type :reader asserted-type)
   (%datum :initarg :datum :reader datum)
   (%asserted-by :initarg :asserted-by :reader asserted-by)))
