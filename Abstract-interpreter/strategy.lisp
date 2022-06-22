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

;;; Is this a thing where we need to use a widening operator instead of the usual?
(defgeneric widening-point-p (strategy thing))
