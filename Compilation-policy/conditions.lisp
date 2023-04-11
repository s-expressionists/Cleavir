(cl:in-package #:cleavir-compilation-policy)

(define-condition bad-optimize-value
    (warning acclimation:condition)
  ((%specifier :initarg :specifier :reader specifier)
   (%expected-type :initarg :expected :reader expected-type)))

(define-condition unknown-optimize-quality
    (warning acclimation:condition)
  ((%specifier :initarg :specifier :reader specifier)))

(define-condition no-policy-computer
    (error acclimation:condition)
  ((%client :initarg :client :reader client)
   (%quality :initarg :quality :reader quality)))
