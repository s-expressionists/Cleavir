(in-package #:cleavir-abstract-interpreter)

(defclass type (forward-values-data)
  ((%system :initarg :system :reader system)))

(defmethod sv-subinfop ((domain type) ty1 ty2)
  (ctype:subtypep ty1 ty2 (system domain)))
(defmethod sv-join/2 ((domain type) ty1 ty2)
  (ctype:disjoin (system domain) ty1 ty2))
(defmethod sv-wjoin/2 ((domain type) ty1 ty2)
  (ctype:wdisjoin (system domain) ty1 ty2))
(defmethod sv-meet/2 ((domain type) ty1 ty2)
  (ctype:conjoin (system domain) ty1 ty2))
(defmethod sv-infimum ((domain type)) (ctype:bottom (system domain)))
(defmethod sv-supremum ((domain type)) (ctype:top (system domain)))
(defmethod values-info ((domain type) required optional rest)
  (ctype:values required optional rest (system domain)))
(defmethod values-required ((domain type) vtype)
  (ctype:values-required vtype (system domain)))
(defmethod values-optional ((domain type) vtype)
  (ctype:values-optional vtype (system domain)))
(defmethod values-rest ((domain type) vtype)
  (ctype:values-rest vtype (system domain)))

;;; Use ctype values-conjoin to get strictness, i.e. that any required type
;;; being bottom means the type as a whole is bottom.
(defmethod meet/2 ((domain type) vty1 vty2)
  (ctype:values-conjoin (system domain) vty1 vty2))

(defgeneric derive-return-type (identity argstype system))
(defmethod derive-return-type (identity argstype system)
  (declare (ignore identity argstype))
  (ctype:values-top system))

(defmethod flow-known-call ((domain type) identity info)
  (derive-return-type identity info (system domain)))

(defmethod flow-instruction ((domain type) (inst bir:constant-reference) &rest in-infos)
  (declare (ignore in-infos))
  (let ((sys (system domain)))
     (ctype:single-value
      (ctype:member sys (bir:constant-value (bir:input inst)))
      sys)))

(defmethod flow-instruction ((domain type) (inst bir:typeq-test) &rest in-infos)
  (destructuring-bind (itype) in-infos
    (let* ((sys (system domain))
           (ivtype (ctype:primary itype sys))
           (ttype (bir:test-ctype inst))
           (nttype (ctype:negate ttype sys)))
      (ctype:single-value
       (if (ctype:disjointp ivtype ttype sys)
           (if (ctype:disjointp ivtype nttype sys)
               (ctype:bottom sys)
               (ctype:member sys nil))
           (if (ctype:disjointp ivtype nttype sys)
               (ctype:negate (ctype:member sys nil) sys)
               (ctype:top sys)))
       sys))))

(defmethod flow-instruction ((domain type) (inst bir:eq-test) &rest in-infos)
  (destructuring-bind (i1type i2type) in-infos
    (let* ((sys (system domain))
           (i1vtype (ctype:primary i1type sys))
           (i2vtype (ctype:primary i2type sys)))
      (ctype:single-value
       (if (ctype:disjointp i1vtype i2vtype sys)
           (ctype:member sys nil)
           (ctype:top sys))
       sys))))

;;;

(defclass asserted-type (type) ())

(defmethod flow-instruction ((domain asserted-type) (inst bir:thei)
                             &rest in-infos)
  (destructuring-bind (inp-info) in-infos
    (ctype:values-conjoin (system domain) (bir:asserted-type inst) inp-info)))

;;;

(defclass derived-type (type)
  ((system :initarg :system :reader system)))

(defmethod flow-instruction ((domain derived-type) (inst bir:thei) &rest in-infos)
  (destructuring-bind (ctype) in-infos
    (if (eq (bir:type-check-function inst) nil)
        ctype
        (ctype:values-conjoin (system domain) (bir:asserted-type inst) ctype))))
