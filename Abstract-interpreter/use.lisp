(in-package #:cleavir-abstract-interpreter)

(defclass usage (backward-values-data) ())

(defclass use () ())
(defclass unused (use) ())
(defclass low-bits (use) ; used for modular arithmetic
  ((%nbits :initarg :nbits :reader use-nbits :type (integer 0))))
(defclass bool-use (use) ()) ; used as a boolean, e.g. input to IF
(defclass all-use (use) ()) ; used arbitrarily

(defmethod sv-subinfop ((domain usage) (i1 unused) (i2 use)) (values t t))
(defmethod sv-subinfop ((domain usage) (i1 use) (i2 all-use)) (values t t))
(defmethod sv-subinfop ((domain usage) (i1 low-bits) (i2 low-bits))
  (values (<= (use-nbits i1) (use-nbits i2)) t))
(defmethod sv-subinfop ((domain usage) (i1 bool-use) (i2 bool-use)) (values t t))
(defmethod sv-subinfop ((domain usage) (i1 low-bits) (i2 bool-use)) (values nil t))
(defmethod sv-subinfop ((domain usage) (i1 bool-use) (i2 low-bits)) (values nil t))

(defmethod sv-join/2 ((domain usage) (i1 unused) (i2 use)) i2)
(defmethod sv-join/2 ((domain usage) (i1 use) (i2 unused)) i1)
(defmethod sv-join/2 ((domain usage) (i1 use) (i2 all-use)) i2)
(defmethod sv-join/2 ((domain usage) (i1 all-use) (i2 use)) i1)
(defmethod sv-join/2 ((domain usage) (i1 low-bits) (i2 low-bits))
  (if (<= (use-nbits i1) (use-nbits i2)) i2 i1))
(defmethod sv-join/2 ((domain usage) (i1 bool-use) (i2 bool-use)) i1)
(defmethod sv-join/2 ((domain usage) (i1 low-bits) (i2 bool-use)) (make-instance 'all-use))
(defmethod sv-join/2 ((domain usage) (i1 bool-use) (i2 low-bits)) (make-instance 'all-use))

(defmethod sv-meet/2 ((domain usage) (i1 unused) (i2 use)) i1)
(defmethod sv-meet/2 ((domain usage) (i1 use) (i2 unused)) i2)
(defmethod sv-meet/2 ((domain usage) (i1 use) (i2 all-use)) i1)
(defmethod sv-meet/2 ((domain usage) (i1 all-use) (i2 use)) i2)
(defmethod sv-meet/2 ((domain usage) (i1 low-bits) (i2 low-bits))
  (if (<= (use-nbits i1) (use-nbits i2)) i1 i2))
(defmethod sv-meet/2 ((domain usage) (i1 bool-use) (i2 bool-use)) i1)
(defmethod sv-meet/2 ((domain usage) (i1 low-bits) (i2 bool-use)) (make-instance 'unused))
(defmethod sv-meet/2 ((domain usage) (i1 bool-use) (i2 low-bits)) (make-instance 'unused))

(defmethod sv-infimum ((domain usage)) (make-instance 'unused))
(defmethod sv-supremum ((domain usage)) (make-instance 'all-use))

(defmethod flow-instruction ((domain usage) (inst bir:ifi) &rest infos)
  (declare (ignore infos))
  (single-value domain (make-instance 'bool-use)))
