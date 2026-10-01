(in-package #:cleavir-derive-cl)

(define-deriver (write domain:type) (client (object &rest keys))
  (declare (ignore keys))
  (ctype:single-value object client))

(define-deriver (prin1 domain:type) (client (object &optional stream))
  (declare (ignore stream))
  (ctype:single-value object client))
(define-deriver (princ domain:type) (client (object &optional stream))
  (declare (ignore stream))
  (ctype:single-value object client))
(define-deriver (print domain:type) (client (object &optional stream))
  (declare (ignore stream))
  (ctype:single-value object client))

(define-deriver (pprint domain:type) (client (object &optional stream))
  (declare (ignore stream))
  (ctype:values () () (ctype:bottom client) client))
