(in-package #:cleavir-abstract-interpreter)

;;;; In the theory of abstract interpretation, a product domain is the
;;;; Cartesian product of other domains. This provides the formalism for
;;;; performing multiple kinds of analysis simultaneously.
;;;; You can also have a _reduced_ product, where information from one domain
;;;; can be used to improve the information in another. For example, if you're
;;;; tracking both the ranges and parity of integer data, and know that a value
;;;; is both odd and in the range 4-6, you can conclude that it is 5.
;;;; In this system, a reduced product is represented as a set of domains and a
;;;; set of _channels_. A channel is a pseudo-domain object representing how
;;;; one domain is computed in terms of others. FLOW-INSTRUCTION receives
;;;; infos for the input domain(s) and computes infos for the output domain(s).
;;;; At the moment, all channels are one-to-one. More involved channels may be
;;;; necessary later.

;;;; It's worth noting that Cousot's conception of reduced products involves
;;;; reducing the whole thing at once, e.g. taking <odd, 4-6> to <odd, 5-5> in
;;;; the above example. For my purposes I'm not sure that that's necessary,
;;;; but experience is the best teacher.

(defclass channel ()
  ())

(defclass scalar-channel (channel)
  ((%input :initarg :input :reader input :type domain)
   (%output :initarg :output :reader output :type domain)))

(defgeneric inputs (channel))
(defmethod inputs ((channel scalar-channel)) (list (input channel)))

(defgeneric outputs (channel))
(defmethod outputs ((channel scalar-channel)) (list (output channel)))

;;; Is the given DOMAIN an input to CHANNEL?
(defgeneric input-domain-p (channel domain))
(defmethod input-domain-p ((channel scalar-channel) (domain domain))
  (eq (input channel) domain))

(defgeneric output-domain-p (channel domain))
(defmethod output-domain-p ((channel scalar-channel) (domain domain))
  (eq (output channel) domain))

(defmethod instruction-input-info (strategy (channel scalar-channel) instruction)
  (instruction-input-info strategy (input channel) instruction))

;;; A channel where infos are computed with inputs from the input domain plus another.
(defclass coop-channel (channel)
  ((%output :initarg :output :reader output :type domain)
   (%other :initarg :other :reader other :type domain)))

(defmethod inputs ((channel coop-channel)) (list (output channel) (other channel)))
(defmethod outputs ((channel coop-channel)) (list (output channel)))
(defmethod input-domain-p ((channel coop-channel) (domain domain))
  (or (eq domain (output channel)) (eq domain (other channel))))
(defmethod output-domain-p ((channel coop-channel) (domain domain))
  (eq domain (output channel)))

(defmethod instruction-input-info (strategy (channel coop-channel) instruction)
  ;; Inputs for a coop channel are just the main channel's inputs appended to
  ;; those of the other channel. The other channel goes first for the admittedly
  ;; arbitrary reason that it makes known-call.lisp simpler.
  (multiple-value-call #'values
    (instruction-input-info strategy (other  channel) instruction)
    (instruction-input-info strategy (output channel) instruction)))

;;; By default, have FLOW-INSTRUCTION return appropriate suprema.
(defgeneric supreme-outputs (domain instruction))
(defmethod supreme-outputs ((domain forward-data) (inst bir:instruction))
  (values-list (loop with sup = (supremum domain)
                     repeat (length (bir:outputs inst))
                     collect sup)))
(defmethod supreme-outputs ((domain backward-data) (inst bir:instruction))
  (values-list (loop with sup = (supremum domain)
                     repeat (length (bir:inputs inst))
                     collect sup)))
(defmethod supreme-outputs ((domain forward-control) (inst bir:instruction))
  (if (bir:successor inst)
      (supremum domain)
      (values-list (make-list (length (bir:next instruction))
                              :initial-element (supremum domain)))))
(defmethod flow-instruction ((channel scalar-channel) (inst bir:instruction)
                             &rest infos)
  (declare (ignore infos))
  (supreme-outputs (output channel) inst))
(defmethod flow-instruction ((channel coop-channel) (inst bir:instruction) &rest infos)
  (declare (ignore infos))
  (supreme-outputs (output channel) inst))

;;;

(defclass product ()
  ((%domains :initarg :domains :reader domains :type list)
   (%channels :initarg :channels :reader channels :type list)))
