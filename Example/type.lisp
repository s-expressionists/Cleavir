(in-package #:cleavir-example)

(defmethod c-ctype:subtypep ((client example) (ct1 ctype:ctype) (ct2 ctype:ctype))
  (ctype:subctypep ct1 ct2))

(defmethod c-ctype:values-subtypep ((client example) (ct1 ctype:cvalues) (ct2 ctype:cvalues))
  (ctype:subctypep ct1 ct2))

(defmethod c-ctype:top ((client example)) (ctype:top))
(defmethod c-ctype:bottom ((client example)) (ctype:bot))

(defmethod c-ctype:values-top ((client example)) (ctype:values-top))
(defmethod c-ctype:values-bottom ((client example)) (ctype:values-bot))

(defmethod c-ctype:top-p ((client example) (ctype ctype:ctype)) (ctype:top-p ctype))
(defmethod c-ctype:bottom-p ((client example) (ctype ctype:ctype)) (ctype:bot-p ctype))

(defmethod c-ctype:conjoin/2 ((client example) (ct1 ctype:ctype) (ct2 ctype:ctype))
  (ctype:conjoin ct1 ct2))
(defmethod c-ctype:disjoin/2 ((client example) (ct1 ctype:ctype) (ct2 ctype:ctype))
  (ctype:disjoin ct1 ct2))
(defmethod c-ctype:wdisjoin/2 ((client example) (ct1 ctype:ctype) (ct2 ctype:ctype))
  ;; KLUDGE
  (ctype:disjoin ct1 ct2))

(defmethod c-ctype:values-conjoin/2 ((client example) (ct1 ctype:ctype) (ct2 ctype:ctype))
  (ctype:conjoin ct1 ct2))
(defmethod c-ctype:values-disjoin/2 ((client example) (ct1 ctype:ctype) (ct2 ctype:ctype))
  (ctype:disjoin ct1 ct2))

(defmethod c-ctype:negate ((client example) (ctype ctype:ctype)) (ctype:negate ctype))

(defmethod c-ctype:class ((client example) (class class)) (ctype:cclass class))
(defmethod c-ctype:cons ((client example) (car ctype:ctype) (cdr ctype:ctype))
  (ctype:ccons car cdr))

(defmethod c-ctype:consp ((client example) (ctype ctype:ctype)) (typep ctype 'ctype:ccons))
(defmethod c-ctype:cons-car ((client example) (ctype ctype:ccons)) (ctype:ccons-car ctype))
(defmethod c-ctype:cons-cdr ((client example) (ctype ctype:ccons)) (ctype:ccons-cdr ctype))

;;;

(defmethod c-ctype:member ((client example) &rest elems)
  (apply #'ctype:cmember elems))

(defmethod c-ctype:member-p ((client example) (ctype ctype:cmember)) t)
(defmethod c-ctype:member-p ((client example) (ctype ctype:ctype)) nil)
(defmethod c-ctype:member-members ((client example) (ctype ctype:cmember))
  (ctype:cmember-members ctype))

;;;

(defmethod c-ctype:values ((client example) required optional rest)
  (ctype:cvalues required optional rest))
(defmethod c-ctype:values-required ((client example) (vct ctype:cvalues))
  (ctype:cvalues-required vct))
(defmethod c-ctype:values-optional ((client example) (vct ctype:cvalues))
  (ctype:cvalues-optional vct))
(defmethod c-ctype:values-rest ((client example) (vct ctype:cvalues))
  (ctype:cvalues-rest vct))

(defmethod c-ctype:function ((client example) req opt rest keyp keys aokp returns)
  (ctype:cfunction (make-instance 'ctype:lambda-list
                     :required req :optional opt :rest rest :keyp keyp
                     :keys keys :aokp aokp)
                   returns))

(defmethod c-ctype:function-required ((client example) (ctype ctype:cfunction))
  (ctype:lambda-list-required (ctype:cfunction-lambda-list ctype)))
(defmethod c-ctype:function-optional ((client example) (ctype ctype:cfunction))
  (ctype:lambda-list-optional (ctype:cfunction-lambda-list ctype)))
(defmethod c-ctype:function-rest ((client example) (ctype ctype:cfunction))
  (ctype:lambda-list-rest (ctype:cfunction-lambda-list ctype)))
(defmethod c-ctype:function-keys ((client example) (ctype ctype:cfunction))
  (ctype:lambda-list-key (ctype:cfunction-lambda-list ctype)))
(defmethod c-ctype:function-keysp ((client example) (ctype ctype:cfunction))
  (ctype:lambda-list-keyp (ctype:cfunction-lambda-list ctype)))
(defmethod c-ctype:function-allow-other-keys-p ((client example) (ctype ctype:cfunction))
  (ctype:lambda-list-aokp (ctype:cfunction-lambda-list ctype)))
(defmethod c-ctype:function-values ((client example) (ctype ctype:cfunction))
  (ctype:cfunction-returns ctype))
