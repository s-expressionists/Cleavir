(in-package #:cleavir-abstract-interpreter)

;;;; A "known" call is a call to some presumably global function
;;;; that the analyzer knows special information about. For
;;;; example, a type inference engine might know that real EXP is
;;;; monotone and propagate type information accordingly.
;;;; In Cleavir, known calls are mediated through the attributes
;;;; system; a datum can be flagged with attributes that give it
;;;; an "identity", which can be used as a key for the analyzer.
;;;; This file provides a mixin for interpretation domains to use
;;;; this sort of information. The FLOW-KNOWN-CALL generic
;;;; function can be specialized for different domains to impart
;;;; known domain information to the abstract interpreter.

(defclass known-call-mixin (domain) ())

(defclass forward-known-call (known-call-mixin forward-data) ())
(defclass forward-values-known-call (forward-known-call forward-values-data) ())
(defclass backward-known-call (known-call-mixin backward-data) ())
(defclass backward-values-known-call (backward-known-call backward-values-data) ())

;;; Given info for the input to a known function (its arguments in
;;; forward domains, or its return values in backward domains),
;;; return computed info for the result (return values if forward,
;;; arguments if backward).
(defgeneric flow-known-call (strategy domain product identity info))

;;;

(defun %flow-ids-call (strategy domain product ids info)
  (cond ((null ids) (supremum domain))
        ((= (length ids) 1)
         (flow-known-call strategy domain product (first ids) info))
        (t
         (apply #'meet
                domain
                (loop for id in ids
                      collect (flow-known-call strategy domain product
                                               id info))))))

(defun %identities (strategy attr-domain abstract-call)
  (if attr-domain
      (let* ((callee (bir:callee abstract-call))
             (attr (info strategy attr-domain callee)))
        (attributes:identities attr))
      nil))

(defun attribute (product)
  (product-domain-of-type 'attribute product))

(defmethod interpret-instruction ((strategy strategy)
                                  (domain forward-known-call)
                                  (product product) (inst bir:mv-call))
  (let* ((identities (%identities strategy (attribute product) inst))
         (output (bir:output inst))
         (args (second (bir:inputs inst)))
         (argsinfo (info strategy domain args)))
    (flow-datum
     strategy domain output
     (%flow-ids-call strategy domain product identities argsinfo))))

(defmethod interpret-instruction ((strategy strategy)
                                  (domain forward-values-known-call)
                                  (product product) (inst bir:call))
  (let* ((identities (%identities strategy (attribute product) inst))
         (output (bir:output inst))
         (argsinfo
           (values-info domain
                        (loop for arg in (rest (bir:inputs inst))
                              for ct = (info strategy domain arg)
                              collect (primary domain ct))
                        nil (sv-infimum domain))))
    (flow-datum
     strategy domain output
     (%flow-ids-call strategy domain product identities argsinfo))))

(defmethod interpret-instruction ((strategy strategy)
                                  (domain backward-known-call)
                                  (product product) (inst bir:mv-call))
  (let* ((identities (%identities strategy (attribute product) inst))
         (args (second (bir:inputs inst)))
         (outinfo (info strategy domain (bir:output inst))))
    (flow-datum
     strategy domain args
     (%flow-ids-call strategy domain product identities outinfo))))

(defmethod interpret-instruction ((strategy strategy)
                                  (domain backward-values-known-call)
                                  (product product) (inst bir:call))
  (loop with identities = (%identities strategy (attribute product) inst)
        with outinfo = (info strategy domain (bir:output inst))
        with ininfo = (%flow-ids-call strategy domain product identities outinfo)
        with svsup = (sv-supremum domain)
        for arg in (rest (bir:inputs inst))
        for i from 0
        for prim = (info-values-nth domain i ininfo)
        ;; The other values are not used by this call, so we should be able to
        ;; infer them as the inf., which is what any unused data should have.
        ;; Not 100% sure of this though.
        for arginfo = (single-value domain prim)
        do (flow-datum strategy domain arg arginfo)))
