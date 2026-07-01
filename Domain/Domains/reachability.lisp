(in-package #:cleavir-domain)

(defclass reachability (noetherian-mixin domain) ())
(defvar reachability (make-instance 'reachability))

;;; Reachability info is T (maybe reachable) or NIL (not reachable).

(defmethod infimum (client (domain reachability))
  (declare (ignore client))
  nil)
(defmethod supremum (client (domain reachability))
  (declare (ignore client))
  t)
(defmethod subinfop (client (domain reachability) info1 info2)
  (declare (ignore client))
  (values (or info2 (not info1)) t))
(defmethod join/2 (client (domain reachability) info1 info2)
  (declare (ignore client))
  (or info1 info2))
(defmethod meet/2 (client (domain reachability) info1 info2)
  (declare (ignore client))
  (and info1 info2))
