(in-package #:cleavir-abstract-interpreter)

(defclass type (forward-values-data)
  ((%system :initarg :system :reader system)))

(defmethod sv-subinfop ((domain type) ty1 ty2)
  (ctype:subtypep ty1 ty2 (system domain)))
(defmethod sv-join/2 ((domain type) ty1 ty2)
  (ctype:disjoin (system domain) ty1 ty2))
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

(defgeneric derive-return-type (instruction identity argstype system))
(defmethod derive-return-type ((inst bir:abstract-call) identity
                               argstype system)
  (declare (ignore identity argstype))
  (ctype:values-top system))

(defmethod interpret-instruction ((strategy strategy) (domain type)
                                  (inst bir:call))
  (let* (;; FIXME: Better cross-domain access
         (attr (bir:attributes (bir:callee inst)))
         (identities (attributes:identities attr))
         (system (system domain))
         (output (bir:output inst)))
    (flow-datum
     strategy domain output
     (if (null identities)
         (ctype:values-top system)
         (let ((argtype
                 (ctype:values
                  (loop for arg in (rest (bir:inputs inst))
                        for ct = (info strategy domain arg)
                        collect (ctype:primary ct system))
                  nil (ctype:bottom system) system)))
           (if (= (length identities) 1)
               (derive-return-type inst (first identities) argtype system)
               (apply #'ctype:values-conjoin
                      system
                      (loop for id in identities
                            collect (derive-return-type
                                     inst id argtype system)))))))))

(defmethod flow-call ((strategy strategy) (domain type) (function bir:function)
                      info)
  (let* ((system (system domain))
         (req (ctype:values-required info system))
         (opt (ctype:values-optional info system))
         (rest (ctype:values-rest info system))
         (false (ctype:single-value (ctype:member system nil) system))
         (true (ctype:single-value (ctype:negate false system) system))
         (top (ctype:top system))
         (svtop (ctype:single-value (ctype:top system) system)))
    (flet ((next () (or (pop req) (pop opt) rest)))
      (bir:map-lambda-list
       (lambda (state item index)
         (declare (ignore index))
         (ecase state
           ((:required)
            (flow-datum strategy domain item
                        (ctype:single-value (next) system)))
           ((&optional)
            (let ((certainly-provided-p (not (null req)))
                  (n (next)))
              (flow-datum strategy domain (first item) n)
              (flow-datum strategy domain (second item)
                          (cond (certainly-provided-p true)
                                ((ctype:bottom-p n system) false)
                                (t svtop)))))
           ((&rest)
            (flow-datum strategy domain item
                        ;; LIST type
                        (ctype:single-value
                         (ctype:disjoin system false (ctype:cons top top system))
                         system)))
           ((&key)
            ;; FIXME: This is a punt.
            (flow-datum strategy domain (second item) svtop)
            (flow-datum strategy domain (third item) svtop))))
       (bir:lambda-list function)))))

;;;

(defclass asserted-type (type) ())

(defmethod interpret-instruction ((strategy strategy) (domain asserted-type)
                                  (inst bir:thei))
  (flow-datum strategy domain
              (bir:output inst)
              (ctype:values-conjoin (system domain)
                                    (bir:asserted-type inst)
                                    (info strategy domain (bir:input inst)))))

;;;

(defclass derived-type (type)
  ((system :initarg :system :reader system)))

(defmethod interpret-instruction ((strategy strategy) (domain derived-type)
                                  (inst bir:thei))
  (let* ((type-check-function (bir:type-check-function inst))
         (input (bir:input inst))
         (ctype (info strategy domain input)))
    (flow-datum strategy domain
                (bir:output inst)
                (if (eq type-check-function nil)
                    ctype
                    (ctype:values-conjoin (system domain)
                                          (bir:asserted-type inst)
                                          ctype)))))
