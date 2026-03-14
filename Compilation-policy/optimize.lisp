(cl:in-package #:cleavir-compilation-policy)

;;; This generic function takes a client object and returns a
;;; list of allowed optimize qualities.  An optimize quality is
;;; represented as a list (NAME TYPE DEFAULT-VALUE) where NAME is the
;;; name of the optimize quality, TYPE is the type of the values of
;;; this optimize quality, and DEFAULT-VALUE is the default value of
;;; this optimize quality, i.e. the value that is assumed when a
;;; declaration of the form (OPTIMIZE NAME) is encountered.
(defgeneric optimize-qualities (client))

;;; This method is called on the client when no
;;; implementation-specific method has been defined.  It returns the
;;; optimize qualities defined in the HyperSpec.
(defmethod optimize-qualities (client)
  (declare (ignore client))
  '((speed (integer 0 3) 3)
    (debug (integer 0 3) 3)
    (space (integer 0 3) 3)
    (compilation-speed (integer 0 3) 3)
    (safety (integer 0 3) 3)))

;;; Given a normalized OPTIMIZE declaration specifier, get the
;;; value of a quality, with return value like GETHASH.

(defun optimize-value (optimize quality)
  (let ((a (assoc quality optimize)))
    (if a
	(values (second a) t)
	(values nil nil))))

;;; Given an OPTIMIZE declaration specifier in code, return a
;;; normalized specification, so that (optimize space) becomes
;;; (optimize (space 3)). Additionally check that all qualities are
;;; known and have allowed values.
(defgeneric normalize-optimize (client optimize))

(defmethod normalize-optimize (client optimize)
  (let* ((optimize-qualities (optimize-qualities client))
	 (policy-qualities ; policies can also be provided directly
	   (policy-qualities client))
	 (all-qualities (append optimize-qualities policy-qualities))
	 normalized)
    (flet ((collect (spec)
	     ;; only collect the first of each quality.
	     (pushnew spec normalized :key #'car)))
      ;; use a DOLIST instead of LOOP because it gets tricky with
      ;; all the nested conditionals and collecting.
      (dolist (spec optimize normalized)
	(if (consp spec) ; like (optimize (speed 3))
	    (destructuring-bind (name value) spec
	      (let ((info (assoc name all-qualities)))
		(if info
		    (destructuring-bind (name type default) info
		      (declare (ignore name default))
		      (if (typep value type)
			  (collect spec)
			  ;; TODO: add more restarts? This will
			  ;; just ignore the bad spec.
			  (warn 'bad-optimize-value
				:specifier spec
				:expected type)))
		    (warn 'unknown-optimize-quality
			  :specifier spec))))
	    ;; like (optimize speed)
	    (let ((info (assoc spec all-qualities)))
	      (if info
		  (destructuring-bind (name type default) info
		    (declare (ignore name type))
		    (collect (list spec default)))
		  (warn 'unknown-optimize-quality
			:specifier spec))))))))
