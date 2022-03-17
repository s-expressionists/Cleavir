(in-package #:cleavir-abstract-interpreter)

;;; A STRATEGY is used to describe how interpretation proceeds. The strategy
;;; determines how instructions are interpreted and in what order, as well as
;;; how information is associated with program objects.
;;; It does not define the nature of the information itself- that's the domain.
(defclass strategy () ())

;;; The OPTIMISM mixin describes optimistic strategies, i.e. strategies that
;;; start all program objects as being linked to the infimum of the domains.
;;; This can give a better approximation to the least fixed point than the
;;; pessimistic strategy, but means that until interpretation is complete the
;;; information associated with an object may be incorrect.
(defclass optimism (strategy) ())
;;; The PESSIMISM mixin describes pessimistic strategies that start all program
;;; objects as having the supremum of the domains. This can give a worse
;;; approximation than optimism, but on the other hand, at no point is the
;;; information associated with an object incorrect.
(defclass pessimism (strategy) ())

;;; Mark that an instruction needs reinterpretation. This may or may not
;;; result in immediate interpretation, depending on the strategy.
;;; Called for effect.
;;; Note that the marking is not specific to any single domain. This is because
;;; when an instruction is marked, we try to flow all domains, not just the one
;;; that changed. Besides being simpler, this facilitates domains relying on
;;; each other's information to increase their precision.
(defgeneric mark (strategy instruction))

;;; Given info for the input to a function (its arguments in forward domains,
;;; or its return values in backward domains), flow and mark appropriately.
;;; Called for effect.
(defgeneric flow-call (strategy domain function info))

;;; Perform abstract interpretation on a module.
(defgeneric interpret-module (strategy domains module))

;;; Perform abstract interpretation of an instruction. This should result in
;;; info changes and marking if there is better information. Called for effect.
(defgeneric interpret-instruction (strategy domain instruction))

;;; Access the information in a domain for a given program object.
(defgeneric info (strategy domain object))
(defgeneric (setf info) (new-info strategy domain object)
  (:argument-precedence-order strategy domain object new-info))
