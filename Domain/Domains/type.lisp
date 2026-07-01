(in-package #:cleavir-domain)

;;;; Domains with CL type information.

(defclass type (values-mixin domain) ())
(defvar type (make-instance 'type))

(defmethod infimum (client (domain type)) (ctype:bottom client))
(defmethod supremum (client (domain type)) (ctype:top client))
(defmethod sv-subinfop (client (domain type) ty1 ty2)
  (ctype:subtypep ty1 ty2 client))
(defmethod sv-join/2 (client (domain type) ty1 ty2)
  (ctype:disjoin client ty1 ty2))
(defmethod sv-meet/2 (client (domain type) ty1 ty2)
  (ctype:conjoin client ty1 ty2))
;; widen: TODO
;; we need to worry about expanding intervals and disjunctions, including as
;; components of a greater type (e.g. (and foo (or bar baz ...)))

(defmethod values-required (client (domain type) vtype)
  (ctype:values-required vtype client))
(defmethod values-optional (client (domain type) vtype)
  (ctype:values-optional vtype client))
(defmethod values-reset (client (domain type) vtype)
  (ctype:values-rest vtype client))

;;; Use ctype values-conjoin to get strictness, i.e. that any required type
;;; being bottom means the type as a whole is bottom.
(defmethod meet/2 (client (domain type) vty1 vty2)
  (ctype:values-conjoin client vty1 vty2))
