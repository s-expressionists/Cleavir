(cl:in-package #:cleavir-cst-to-ast)

(defun variable-info (system environment var-name-cst)
  (let* ((symbol (cst:raw var-name-cst))
         (info (trucler:describe-variable system environment symbol)))
    (loop while (null info)
	  do (restart-case (error 'no-variable-info
				  :name symbol
                                  :cst var-name-cst)
	       (continue ()
		 :report (lambda (stream)
			   (format stream "Consider the variable as special."))
                 (setf info
                       (make-instance 'trucler:special-variable-description
                         :name symbol)))
               ;; This is identical to CONTINUE, but more specifically named.
	       (consider-special ()
		 :report (lambda (stream)
			   (format stream "Consider the variable as special."))
                 (setf info
                       (make-instance 'trucler:special-variable-description
                         :name symbol)))
	       (substitute (new-symbol)
		 :report (lambda (stream)
			   (format stream "Substitute a different name."))
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read *query-io*)))
		 (setq info (trucler:describe-variable
                             system environment new-symbol)))))
    info))

(defun function-info (system environment function-name-cst)
  (let* ((function-name (cst:raw function-name-cst))
         (result (trucler:describe-function system environment function-name)))
    (loop while (null result)
	  do (restart-case (error 'no-function-info
				  :name function-name
                                  :cst function-name-cst)
	       (consider-global ()
		 :report (lambda (stream)
			   (format stream
				   "Treat it as the name of a global function."))
		 (return-from function-info
		   (make-instance 'trucler:global-function-description
		     :name function-name)))
	       (substitute (new-function-name)
		 :report (lambda (stream)
			   (format stream "Substitute a different name."))
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read *query-io*)))
		 (setq result (trucler:describe-function
                               system environment new-function-name)))))
    result))

(defun tag-info (system environment tag-name-cst)
  (let* ((tag-name (cst:raw tag-name-cst))
         (result (trucler:describe-tag system environment tag-name)))
    (loop while (null result)
	  do (restart-case (error 'no-tag-info
				  :name tag-name
                                  :cst tag-name-cst)
	       (substitute (new-tag-name)
		 :report (lambda (stream)
			   (format stream "Substitute a different name."))
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read *query-io*)))
		 (setq result (trucler:describe-tag
                               system environment new-tag-name)))))
    result))

(defun block-info (system environment block-name-cst)
  (let* ((block-name (cst:raw block-name-cst))
         (info (trucler:describe-block system environment block-name)))
    (loop while (null info)
          do (restart-case (error 'no-block-info
                                  :name block-name
                                  :cst block-name-cst)
               (substitute (new-block-name)
                 :report (lambda (stream)
                           (format stream "Substitute a different name."))
                 :interactive (lambda ()
                                (format *query-io* "Enter new name: ")
                                (list (read *query-io*)))
                 (setq info (trucler:describe-block
                             system environment new-block-name)))))
    info))
