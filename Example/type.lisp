(in-package #:cleavir-example)

(defmethod c-ctype:subtypep ((ct1 ctype:ctype) (ct2 ctype:ctype) (system example))
  (ctype:subctypep ct1 ct2))

(defmethod c-ctype:values-subtypep ((ct1 ctype:cvalues) (ct2 ctype:cvalues) (sys example))
  (ctype:subctypep ct1 ct2))

(defmethod c-ctype:top ((sys example)) (ctype:top))
(defmethod c-ctype:bottom ((sys example)) (ctype:bot))

(defmethod c-ctype:values-top ((sys example)) (ctype:values-top))
(defmethod c-ctype:values-bottom ((sys example)) (ctype:values-bot))

(defmethod c-ctype:top-p ((ctype ctype:ctype) (sys example)) (ctype:top-p ctype))
(defmethod c-ctype:bottom-p ((ctype ctype:ctype) (sys example)) (ctype:bot-p ctype))

(defmethod c-ctype:conjoin/2 ((ct1 ctype:ctype) (ct2 ctype:ctype) (sys example))
  (ctype:conjoin ct1 ct2))
(defmethod c-ctype:disjoin/2 ((ct1 ctype:ctype) (ct2 ctype:ctype) (sys example))
  (ctype:disjoin ct1 ct2))
(defmethod c-ctype:wdisjoin/2 ((ct1 ctype:ctype) (ct2 ctype:ctype) (sys example))
  ;; KLUDGE
  (ctype:disjoin ct1 ct2))

(defmethod c-ctype:values-conjoin/2 ((ct1 ctype:ctype) (ct2 ctype:ctype) (sys example))
  (ctype:conjoin ct1 ct2))
(defmethod c-ctype:values-disjoin/2 ((ct1 ctype:ctype) (ct2 ctype:ctype) (sys example))
  (ctype:disjoin ct1 ct2))

(defmethod c-ctype:negate ((ctype ctype:ctype) (sys example)) (ctype:negate ctype))

(defmethod c-ctype:class ((class class) (sys example)) (ctype:cclass class))
(defmethod c-ctype:cons ((car ctype:ctype) (cdr ctype:ctype) (sys example))
  (ctype:ccons car cdr))

(defmethod c-ctype:consp ((ctype ctype:ctype) (sys example)) (typep ctype 'ctype:ccons))
(defmethod c-ctype:cons-car ((ctype ctype:ccons) (sys example)) (ctype:ccons-car ctype))
(defmethod c-ctype:cons-cdr ((ctype ctype:ccons) (sys example)) (ctype:ccons-cdr ctype))

;;;

(defmethod c-ctype:member ((sys example) &rest elems)
  (apply #'ctype:cmember elems))

(defmethod c-ctype:member-p ((sys example) (ctype ctype:cmember)) t)
(defmethod c-ctype:member-p ((sys example) (ctype ctype:ctype)) nil)
(defmethod c-ctype:member-members ((sys example) (ctype ctype:cmember))
  (ctype:cmember-members ctype))

;;;

(defmethod c-ctype:values (required optional rest (sys example))
  (ctype:cvalues required optional rest))
(defmethod c-ctype:values-required ((vct ctype:cvalues) (sys example))
  (ctype:cvalues-required vct))
(defmethod c-ctype:values-optional ((vct ctype:cvalues) (sys example))
  (ctype:cvalues-optional vct))
(defmethod c-ctype:values-rest ((vct ctype:cvalues) (sys example))
  (ctype:cvalues-rest vct))

(defmethod c-ctype:function (req opt rest keyp keys aokp returns (sys example))
  (ctype:cfunction (make-instance 'ctype:lambda-list
                     :required req :optional opt :rest rest :keyp keyp
                     :keys keys :aokp aokp)
                   returns))

(defmethod c-ctype:function-required ((ctype ctype:cfunction) (sys example))
  (ctype:lambda-list-required (ctype:cfunction-lambda-list ctype)))
(defmethod c-ctype:function-optional ((ctype ctype:cfunction) (sys example))
  (ctype:lambda-list-optional (ctype:cfunction-lambda-list ctype)))
(defmethod c-ctype:function-rest ((ctype ctype:cfunction) (sys example))
  (ctype:lambda-list-rest (ctype:cfunction-lambda-list ctype)))
(defmethod c-ctype:function-keys ((ctype ctype:cfunction) (sys example))
  (ctype:lambda-list-key (ctype:cfunction-lambda-list ctype)))
(defmethod c-ctype:function-keysp ((ctype ctype:cfunction) (sys example))
  (ctype:lambda-list-keyp (ctype:cfunction-lambda-list ctype)))
(defmethod c-ctype:function-allow-other-keys-p ((ctype ctype:cfunction) (sys example))
  (ctype:lambda-list-aokp (ctype:cfunction-lambda-list ctype)))
(defmethod c-ctype:function-values ((ctype ctype:cfunction) (sys example))
  (ctype:cfunction-returns ctype))
