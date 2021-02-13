(in-package #:cleavir-conditions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ORIGIN
;;;
;;; This returns the source position a condition originated from.
(defgeneric origin (condition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition type ORIGIN
;;;
;;; ORIGIN is a convenience mixin for condition types that directly store their
;;; origin in a slot.
(define-condition origin (acclimation:condition)
  ((%origin :initarg :origin :reader origin)))
