(in-package #:cleavir-domain)

(defclass attribute (noetherian-values-mixin domain) ())

;;; Lattice operations are reversed because we start with the optimistic
;;; assumption and move up the lattice.
(defmethod sv-subinfop (client (domain attribute) attr1 attr2)
  (declare (ignore client))
  (values (attributes:sub-attributes-p attr2 attr1) t))
(defmethod sv-join/2 (client (domain attribute) attr1 attr2)
  (declare (ignore client))
  (attributes:meet-attributes attr1 attr2))
(defmethod sv-meet/2 (client (domain attribute) attr1 attr2)
  (declare (ignore client))
  (attributes:join-attributes attr1 attr2))
(defmethod sv-infimum (client (domain attribute)) (declare (ignore client)) t)
(defmethod sv-supremum (client (domain attribute)) (declare (ignore client)) nil)
