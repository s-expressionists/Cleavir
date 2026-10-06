(in-package #:cleavir-domain)

;;;; In the theory of abstract interpretation, a product domain is the
;;;; Cartesian product of other domains. This provides the formalism for
;;;; performing multiple kinds of analysis simultaneously.

;;;; You can also have a _reduced_ product, where information from one domain
;;;; can be used to improve the information in another. For example, if you're
;;;; tracking both the ranges and parity of integer data, and know that a value
;;;; is both odd and in the range 4-6, you can conclude that it is 5.
;;;; In this system, a reduced product is represented as a set of domains and a
;;;; set of _channels_. A channel is a pseudo-domain object representing how
;;;; one domain is computed in terms of others.
;;;; Channels aren't actually implemented yet.

;;;; It's worth noting that Cousot's conception of reduced products involves
;;;; reducing the whole thing at once, e.g. taking <odd, 4-6> to <odd, 5-5> in
;;;; the above example. For my purposes I'm not sure that that's necessary,
;;;; but experience is the best teacher.

(defclass product (domain)
  ((%domains :initarg :domains :reader domains :type list)
   #+(or)
   (%channels :initarg :channels :reader channels :type list)))

(defclass product-info ()
  (;; A list of infos as long as the product's list of domains.
   ;; The first info is for the first domain, etc.
   (%infos :initarg :infos :reader infos :type list)))

;;; Get the info for a given domain out of an info.
;;; If the info's domain isn't actually a product, get the info if the
;;; requested domain matches the input domain, otherwise return the supremum.
;;; If it is a product but doesn't contain info for the requested domain,
;;; return the supremum.
(defun project (client product domain info)
  (cond ((typep product 'product)
         (let ((n (position domain (domains product))))
           (if n
               (nth n (infos info))
               (supremum client domain))))
        ((eq product domain) info)
        (t (supremum client domain))))

;;; Make a product info for the given product domain out of the given infos.
;;; Reduction is done automatically. Or will be once I implement it - TODO.
(defun product (client product infos)
  (declare (ignore client product))
  (make-instance 'product-info :infos infos))

(defmethod infimum (client (product product))
  (product client product
           (loop for d in (domains product) collecting (infimum client d))))
(defmethod supremum (client (product product))
  (product client product
           (loop for d in (domains product) collecting (supremum client d))))
(defmethod subinfop (client (product product)
                     (info1 product-info) (info2 product-info))
  (loop with sub = t with surety = t
        for domain in (domains product)
        for i1 in (infos info1) for i2 in (infos info2)
        do (multiple-value-bind (ssub ssurety)
               (subinfop client domain i1 i2)
             (cond ((not ssurety) ; nil nil
                    (setf sub nil surety nil))
                   ((not ssub) ; nil t
                    ;; if one domain is not subinfo, the product as a whole
                    ;; definitely isn't, regardless of the other domains.
                    (return-from subinfop (values nil t)))))
        finally (return (values sub surety))))
(defmethod join/2 (client (product product)
                   (info1 product-info) (info2 product-info))
  (product client product
           (loop for d in (domains product)
                 for i1 in (infos info1) for i2 in (infos info2)
                 collecting (join/2 client d i1 i2))))
(defmethod meet/2 (client (product product)
                   (info1 product-info) (info2 product-info))
  (product client product
           (loop for d in (domains product)
                 for i1 in (infos info1) for i2 in (infos info2)
                 collect (meet/2 client d i1 i2))))
(defmethod widen (client (product product) (old product-info) (new product-info))
  (product client product
           (loop for d in (domains product)
                 for i1 in (infos old) for i2 in (infos new)
                 collect (widen client d i1 i2))))
