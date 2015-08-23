(cl:in-package #:cleavir-simple-value-numbering)

;;; We attribute a unique number to each value designator.  That way,
;;; we can keep the value designators in a partition sorted by the
;;; unique number which makes operations on partitions faster.
;;;
;;; This variable should be bound to some small integer at the
;;; beginning of processing.
(defvar *unique-number*)

;;; We use instances of this class to designate values.
(defclass value-designator ()
  ((%unique-number :initform (incf *unique-number*) :reader unique-number)))

;;; This variable holds and EQ hash table mapping constants to value
;;; designators.
(defvar *constant-designators*)

(defun constant-designator (constant)
  (let ((result (gethash constant *constant-designators*)))
    (when (null result)
      (setf result (make-instance 'value-designator))
      (setf (gethash constant *constant-designators*) result))
    result))

(defun simple-value-numbering (initial-instruction)
  (declare (ignore initial-instruction))
  (let ((*unique-number* 0)
	(*constant-designators* (make-hash-table :test #'eq)))))

;;  LocalWords:  designator designators
