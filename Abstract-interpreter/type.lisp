(in-package #:cleavir-abstract-interpreter)

(defclass type (forward-values-data)
  ((%client :initarg :client :reader client)))

(defmethod sv-subinfop ((domain type) ty1 ty2)
  (ctype:subtypep (client domain) ty1 ty2))
(defmethod sv-join/2 ((domain type) ty1 ty2)
  (ctype:disjoin (client domain) ty1 ty2))
(defmethod sv-wjoin/2 ((domain type) ty1 ty2)
  (ctype:wdisjoin (client domain) ty1 ty2))
(defmethod sv-meet/2 ((domain type) ty1 ty2)
  (ctype:conjoin (client domain) ty1 ty2))
(defmethod sv-infimum ((domain type)) (ctype:bottom (client domain)))
(defmethod sv-supremum ((domain type)) (ctype:top (client domain)))
(defmethod values-info ((domain type) required optional rest)
  (ctype:values (client domain) required optional rest))
(defmethod values-required ((domain type) vtype)
  (ctype:values-required (client domain) vtype))
(defmethod values-optional ((domain type) vtype)
  (ctype:values-optional (client domain) vtype))
(defmethod values-rest ((domain type) vtype)
  (ctype:values-rest (client domain) vtype))

;;; Use ctype values-conjoin to get strictness, i.e. that any required type
;;; being bottom means the type as a whole is bottom.
(defmethod meet/2 ((domain type) vty1 vty2)
  (ctype:values-conjoin (client domain) vty1 vty2))

(defgeneric derive-return-type (client identity argstype))
(defmethod derive-return-type (client identity argstype)
  (declare (ignore identity argstype))
  (ctype:values-top client))

(defmethod flow-known-call ((domain type) identity info)
  (derive-return-type (client domain) identity info))

(defmethod flow-instruction ((domain type) (inst bir:constant-reference) &rest in-infos)
  (declare (ignore in-infos))
  (let ((client (client domain)))
     (ctype:single-value
      client
      (ctype:member client (bir:constant-value (bir:input inst))))))

(defmethod flow-instruction ((domain type) (inst bir:typeq-test) &rest in-infos)
  (destructuring-bind (itype) in-infos
    (let* ((client (client domain))
           (ivtype (ctype:primary client itype))
           (ttype (bir:test-ctype inst))
           (nttype (ctype:negate client ttype)))
      (ctype:single-value
       client
       (if (ctype:disjointp client ivtype ttype)
           (if (ctype:disjointp client ivtype nttype)
               (ctype:bottom client)
               (ctype:member client nil))
           (if (ctype:disjointp client ivtype nttype)
               (ctype:negate client (ctype:member client nil))
               (ctype:top client)))))))

(defmethod flow-instruction ((domain type) (inst bir:eq-test) &rest in-infos)
  (destructuring-bind (i1type i2type) in-infos
    (let* ((client (client domain))
           (i1vtype (ctype:primary client i1type))
           (i2vtype (ctype:primary client i2type)))
      (ctype:single-value
       client
       (if (ctype:disjointp client i1vtype i2vtype)
           (ctype:member client nil)
           (ctype:top client))))))

;;;

(defclass asserted-type (type) ())

(defmethod flow-instruction ((domain asserted-type) (inst bir:thei)
                             &rest in-infos)
  (destructuring-bind (inp-info) in-infos
    (ctype:values-conjoin (client domain) (bir:asserted-type inst) inp-info)))

;;;

(defclass derived-type (type)
  ((%client :initarg :client :reader client)))

(defmethod flow-instruction ((domain derived-type) (inst bir:thei) &rest in-infos)
  (destructuring-bind (ctype) in-infos
    (if (eq (bir:type-check-function inst) nil)
        ctype
        (ctype:values-conjoin (client domain) (bir:asserted-type inst) ctype))))
