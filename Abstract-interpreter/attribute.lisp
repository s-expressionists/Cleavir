(in-package #:cleavir-abstract-interpreter)

(defclass attribute (forward-values-data) ())

(defclass attribute-values ()
  ((%required :initarg :required :reader required-attr)
   (%optional :initarg :optional :reader optional-attr)
   (%rest :initarg :rest :reader rest-attr)))

;;; Lattice operations are reversed because we start with the optimistic
;;; assumption and move up the lattice.
(defmethod sv-subinfop ((domain attribute) attr1 attr2)
  (values (attributes:sub-attributes-p attr2 attr1) t))
(defmethod sv-join/2 ((domain attribute) attr1 attr2)
  (attributes:meet-attributes attr1 attr2))
(defmethod sv-infimum ((domain attribute)) t)
(defmethod sv-supremum ((domain attribute)) nil)
(defmethod values-info ((domain attribute) required optional rest)
  (make-instance 'attribute-values
    :required required :optional optional :rest rest))
(defmethod values-required ((domain attribute) (vattr attribute-values))
  (required-attr vattr))
(defmethod values-optional ((domain attribute) (vattr attribute-values))
  (optional-attr vattr))
(defmethod values-rest ((domain attribute) (vattr attribute-values))
  (rest-attr vattr))
