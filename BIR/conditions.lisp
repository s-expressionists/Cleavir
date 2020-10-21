(in-package #:cleavir-bir)

(define-condition unused-variable (style-warning acclimation:condition)
  ((%variable :initarg :variable :reader variable)))
