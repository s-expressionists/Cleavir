(cl:in-package #:cleavir-cst-to-ast)

(defun function-info (environment function-name-cst)
  (let* ((function-name (cst:raw function-name-cst))
         (result (cleavir-env:function-info environment function-name)))
    (loop while (null result)
	  do (restart-case (error 'cleavir-env:no-function-info
				  :name function-name
                                  :origin (cst:source function-name-cst))
	       (consider-global ()
		 :report (lambda (stream)
			   (format stream
				   "Treat it as the name of a global function."))
		 (return-from function-info
		   (make-instance 'cleavir-env:global-function-info
		     :name function-name)))
	       (substitute (new-function-name)
		 :report (lambda (stream)
			   (format stream "Substitute a different name."))
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read *query-io*)))
		 (setq result (cleavir-env:function-info environment new-function-name)))))
    result))

(defun tag-info (environment tag-name-cst)
  (let* ((tag-name (cst:raw tag-name-cst))
         (result (cleavir-env:tag-info environment tag-name)))
    (loop while (null result)
	  do (restart-case (error 'cleavir-env:no-tag-info
				  :name tag-name
                                  :origin (cst:source tag-name-cst))
	       (substitute (new-tag-name)
		 :report (lambda (stream)
			   (format stream "Substitute a different name."))
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read *query-io*)))
		 (setq result (cleavir-env:tag-info environment new-tag-name)))))
    result))

;;; FIXME: Needs cleanup and to be moved.  This function takes a
;;; (FUNCTION ...) type specifier and returns required, optional,
;;; restp, rest, keysp, keys, allow-other-keys-p, and values as
;;; values.  We could signal warnings/errors on malformed function
;;; types, but we're getting these from the client, which might want
;;; to do its own validation, ahead of time.
(defun parse-function-type (ftype)
  (if (and (consp ftype)
           (eq (car ftype) 'cl:function))
      (destructuring-bind (&optional (lambda-list '*) (values '*)) (cdr ftype)
        (if (eq lambda-list '*)
            (values nil nil t t nil nil nil values)
            (loop with state = :required
                  with required with optional with restp
                  with rest with keysp with keys with aok-p
                  for item in lambda-list
                  do (case item
                       ((&optional)
                        (assert (eq state :required))
                        (setf state '&optional))
                       ((&rest)
                        (assert (member state '(:required &optional)))
                        (setf state '&rest))
                       ((&key)
                        (assert (member state '(:required &optional &rest)))
                        (setf state '&key keysp t))
                       ((&allow-other-keys)
                        (assert (eq state '&key))
                        (setf state '&allow-other-keys aok-p t))
                       (t ; actual type thing
                        (ecase state
                          ((:required) (push item required))
                          ((&optional) (push item optional))
                          ((&rest)
                           (assert (not restp))
                           (setf restp t)
                           (setf rest item))
                          ((&key)
                           ;; Syntax check: Should be (keyword type)
                           (assert (and (consp item)
                                        (consp (cdr item))
                                        (null (cddr item))))
                           (push (copy-list item) keys)))))
                  finally
                     (return (values (nreverse required) (nreverse optional)
                                     restp rest keysp (nreverse keys) aok-p values)))))
      (values nil nil t t nil nil nil '(values &rest t))))
