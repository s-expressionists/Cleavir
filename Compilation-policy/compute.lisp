(cl:in-package #:cleavir-compilation-policy)

;;; Compute the value of a policy quality based on OPTIMIZE info.
;;; NAME is the name of the quality.
;;; Should return the value.
(defgeneric compute-policy-quality (client name optimize))

;;; If a policy is directly specified, just use that.
(defmethod compute-policy-quality :around
    (client name optimize)
  (multiple-value-bind (value present-p)
      (optimize-value optimize name)
    (if present-p
	value
	(call-next-method))))

(defmethod compute-policy-quality (client name optimize)
  (declare (ignore optimize))
  (error 'no-policy-computer :quality name :client client))

;;; Compute the entire policy for given OPTIMIZE info.
;;; This is a generic so that in the future an implementation could
;;; hypothetically override the whole process; however, doing so
;;; would take more understanding of POLICY objects than is
;;; presently external.
(defgeneric compute-policy (client optimize))

;;; Default method for the usual case of the implementation not
;;; overriding the entire process.
(defmethod compute-policy (client optimize)
  (let ((policy-qualities (policy-qualities client))
	(optimize (normalize-optimize client optimize)))
    ;; uses representation of policies as alists
    (loop for (name) in policy-qualities ; ignore CDR
	  collect (cons name (compute-policy-quality
			      client name optimize)))))
