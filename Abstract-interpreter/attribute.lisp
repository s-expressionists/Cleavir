(in-package #:cleavir-abstract-interpreter)

(defclass attribute (noetherian-mixin forward-values-data) ())

;;; Lattice operations are reversed because we start with the optimistic
;;; assumption and move up the lattice.
(defmethod sv-subinfop ((domain attribute) attr1 attr2)
  (values (attributes:sub-attributes-p attr2 attr1) t))
(defmethod sv-join/2 ((domain attribute) attr1 attr2)
  (attributes:meet-attributes attr1 attr2))
(defmethod sv-meet/2 ((domain attribute) attr1 attr2)
  (attributes:join-attributes attr1 attr2))
(defmethod sv-infimum ((domain attribute)) t)
(defmethod sv-supremum ((domain attribute)) nil)

;;; Treat NIL as meaning (&rest nil), and an attribute as meaning
;;; (attr &rest nil).
(defmethod values-required ((domain attribute) (vattr null)) ())
(defmethod values-optional ((domain attribute) (vattr null)) ())
(defmethod values-rest ((domain attribute) (vattr null)) vattr)
(defmethod values-required ((domain attribute) (vattr attributes:attributes))
  (list vattr))
(defmethod values-optional ((domain attribute) (vattr attributes:attributes))
  ())
(defmethod values-rest ((domain attribute) (vattr attributes:attributes))
  nil)

(defmethod subinfop ((domain attribute) (attr1 attributes:attributes)
                     (attr2 attributes:attributes))
  (values (attributes:sub-attributes-p attr2 attr1) t))
(defmethod subinfop ((domain attribute) (attr1 null)
                     (attr2 attributes:attributes))
  (values (attributes:sub-attributes-p attr2 attr1) t))
(defmethod subinfop ((domain attribute) (attr1 attributes:attributes)
                     (attr2 null))
  (values (attributes:sub-attributes-p attr2 attr1) t))
(defmethod subinfop ((domain attribute) (attr1 null) (attr2 null))
  (values (attributes:sub-attributes-p attr2 attr1) t))

(defun attr-subinfop1 (domain attr1 attr2)
  (let* ((required (values-required domain attr2))
         (required-count (length required))
         (optional (values-optional domain attr2))
         (rest (values-rest domain attr2)))
    (cond ((< 1 required-count) (values nil t))
          ((< 1 (+ required-count (length optional))) (values nil nil))
          (t (multiple-value-bind (answer certain)
                 (sv-subinfop domain attr1
                              (or (pop required) (pop optional) rest))
               (if answer
                   (sv-subinfop domain nil
                                (or (pop required) (pop optional) rest))
                   (values nil certain)))))))
(defmethod subinfop ((domain attribute) (attr1 null) (attr2 values-info))
  (attr-subinfop1 domain attr1 attr2))
(defmethod subinfop ((domain attribute) (attr1 attributes:attributes)
                     (attr2 values-info))
  (attr-subinfop1 domain attr1 attr2))

(defun attr-subinfop2 (domain attr1 attr2)
  ;; TODO
  (declare (ignore domain attr1 attr2))
  (values nil nil))
(defmethod subinfop ((domain attribute) (attr1 values-info)
                     (attr2 null))
  (attr-subinfop2 domain attr1 attr2))
(defmethod subinfop ((domain attribute) (attr1 values-info)
                     (attr2 attributes:attributes))
  (attr-subinfop2 domain attr1 attr2))

(defmethod meet/2 ((domain attribute) (attr1 null) (attr2 null))
  (sv-meet/2 domain attr1 attr2))
(defmethod meet/2 ((domain attribute) (attr1 null)
                   (attr2 attributes:attributes))
  (sv-meet/2 domain attr1 attr2))
(defmethod meet/2 ((domain attribute) (attr1 attributes:attributes)
                   (attr2 null))
  (sv-meet/2 domain attr1 attr2))
(defmethod meet/2 ((domain attribute) (attr1 attributes:attributes)
                   (attr2 attributes:attributes))
  (sv-meet/2 domain attr1 attr2))

(defmethod join/2 ((domain attribute) (attr1 null) (attr2 null))
  (sv-join/2 domain attr1 attr2))
(defmethod join/2 ((domain attribute) (attr1 null)
                   (attr2 attributes:attributes))
  (sv-join/2 domain attr1 attr2))
(defmethod join/2 ((domain attribute) (attr1 attributes:attributes)
                   (attr2 null))
  (sv-join/2 domain attr1 attr2))
(defmethod join/2 ((domain attribute) (attr1 attributes:attributes)
                   (attr2 attributes:attributes))
  (sv-join/2 domain attr1 attr2))
