(in-package #:cleavir-abstract-interpreter)

;;;; In the theory of abstract interpretation, a product domain is the
;;;; Cartesian product of other domains. This provides the formalism for
;;;; performing multiple kinds of analysis simultaneously.
;;;; You can also have a _reduced_ product, where information from one domain
;;;; can be used to improve the information in another. For example, if you're
;;;; tracking both the ranges and parity of integer data, and know that a value
;;;; is both odd and in the range 4-6, you can conclude that it is 5.
;;;; In this system, the product of domains is not represented as another
;;;; domain, in order for it to be possible to, for example, store information
;;;; from different domains in different ways. The product is instead provided
;;;; for reduced product purposes, i.e. for domains to communicate with one
;;;; another.
;;;; Methods on interpret-instruction can use the product to get at other
;;;; domain objects, which they can then use to check other domain information
;;;; in order to improve their analyses.

(defclass product ()
  ((%domains :initarg :domains :reader domains :type list)))

(defun product-domain (predicate product)
  (find-if predicate (domains product)))

(defun product-domain-of-type (type product)
  (find-if (lambda (domain) (typep domain type)) (domains product)))
