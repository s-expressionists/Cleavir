(in-package #:cleavir-domain)

;;;; Track equivalence classes among values. Each value is somehow assigned an
;;;; object representing its equivalence class. If two values have EQUIVALENTP
;;;; objects, they are in the same equivalence class.
;;;; The nature of these objects is left up to the user, but they could for
;;;; example be the BIR instructions producing values.
;;;; This domain should be useful for reducing other domain information, and
;;;; sometimes by itself, e.g. we can derive that the type of (- x x) is a
;;;; constant zero regardless of X (other than float weirdness).

;;; Reserved objects for the infimum and supremum.
;;; The supremum is EQL to nothing. The infimum is EQL to everything, which
;;; doesn't make much sense, but that's why it's the infimum.

(defclass equivalence (noetherian-values-mixin domain) ())
(defvar equivalence (make-instance 'equivalence))

(defvar +eql-infimum+ (make-symbol "INFIMUM"))
(defvar +eql-supremum+ (make-symbol "SUPREMUM"))

(defun equivalentp (marker1 marker2)
  (cond ((eql marker1 +eql-infimum+) (not (eql marker2 +eql-supremum+)))
        ((eql marker2 +eql-infimum+) (not (eql marker1 +eql-supremum+)))
        ((or (eql marker1 +eql-supremum+) (eql marker2 +eql-supremum+)) nil)
        ((eql marker1 marker2))))

(defmethod sv-infimum (client (domain equivalence))
  (declare (ignore client))
  +eql-infimum+)
(defmethod sv-supremum (client (domain equivalence))
  (declare (ignore client))
  +eql-supremum+)
(defmethod sv-subinfop (client (domain equivalence) info1 info2)
  (declare (ignore client))
  (values
   (or (eql info1 info2)
       (eql info1 +eql-infimum+)
       (eql info2 +eql-supremum+))
   t))
(defmethod sv-meet/2 (client (domain equivalence) info1 info2)
  (declare (ignore client))
  (if (eql info1 info2) info1 +eql-infimum+))
(defmethod sv-join/2 (client (domain equivalence) info1 info2)
  (declare (ignore client))
  (if (eql info1 info2) info1 +eql-supremum+))
