(in-package #:cleavir-abstract-interpreter)

;;;; A "known" call is a call to some presumably global function
;;;; that the analyzer knows special information about. For
;;;; example, a type inference engine might know that real EXP is
;;;; monotone and propagate type information accordingly.
;;;; In Cleavir, known calls are mediated through the attributes
;;;; system; a datum can be flagged with attributes that give it
;;;; an "identity", which can be used as a key for the analyzer.
;;;; This file provides a channel for interpretation domains to use
;;;; this sort of information. A function provided on channel
;;;; creation computes the known domain information.

(defclass known-call-channel (coop-channel)
  ((%other :type attribute)
   (%flower :initarg :flower :reader known-call-flower :type function)))

(defgeneric known-call-outputs (domain inst info))
(defgeneric known-call-inputs (domain inst &rest infos))

(defmethod known-call-outputs ((domain forward-data) (inst bir:abstract-call) info)
  info)

(defmethod known-call-outputs ((domain forward-control) (inst bir:abstract-call) info)
  info)

(defmethod known-call-outputs ((domain backward-data) (inst bir:mv-call) info)
  ;; mv calls have two inputs, and we need to output an info for both.
  (values (supremum domain) info))
(defmethod known-call-outputs ((domain backward-values-data) (inst bir:call) info)
  ;; Break it up by arguments. Also do the supremum for the callee.
  (apply #'values (supremum domain)
         (loop for arg in (rest (bir:inputs inst))
               for i from 0
               collect (single-value domain (info-values-nth domain i info)))))

(defmethod known-call-inputs ((domain forward-data) (inst bir:mv-call) &rest infos)
  (second infos))
(defmethod known-call-inputs ((domain forward-values-data) (inst bir:call) &rest infos)
  (ftm-info domain (mapcar (lambda (i) (primary domain i)) (rest infos))))

(defmethod known-call-inputs ((domain forward-control) (inst bir:abstract-call)
                              &rest infos)
  (first infos))

(defmethod known-call-inputs ((domain backward-data) (inst bir:abstract-call)
                              &rest infos)
  (first infos))

;;;

(defun %flow-ids-call (flower domain ids info)
  (cond ((null ids) (supremum domain))
        ((= (length ids) 1)
         (funcall flower (first ids) info))
        (t
         (apply #'meet
                domain
                (loop for id in ids
                      collect (funcall flower id info))))))

(defmethod flow-instruction ((channel known-call-channel) (inst bir:mv-call)
                             &rest infos)
  (let* ((nattr (length (bir:inputs inst))) ; number of infos that are attribute domain
         (main-infos (nthcdr nattr infos)) ; infos for the non-attribute domain
         (attr (first infos)) ; attributes for callee
         (ids (attributes:identities attr)))
    (known-call-outputs
     (output channel) inst
     (%flow-ids-call (known-call-flower channel) (output channel) ids
                     (apply #'known-call-inputs (output channel) inst main-infos)))))

;;; identical to the above. We don't use abstract-call because we don't want to handle
;;; local calls.
(defmethod flow-instruction ((channel known-call-channel) (inst bir:call)
                             &rest infos)
  (let* ((nattr (length (bir:inputs inst))) ; number of infos that are attribute domain
         (main-infos (nthcdr nattr infos)) ; infos for the non-attribute domain
         (attr (first infos)) ; attributes for callee
         (ids (attributes:identities attr)))
    (known-call-outputs
     (output channel) inst
     (%flow-ids-call (known-call-flower channel) (output channel) ids
                     (apply #'known-call-inputs (output channel) inst main-infos)))))
