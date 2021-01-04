(cl:in-package #:cleavir-cst-to-ast)

(defun variable-info (environment var-name-cst)
  (let* ((symbol (cst:raw var-name-cst))
         (info (cleavir-env:variable-info environment symbol)))
    (loop while (null info)
	  do (restart-case (error 'no-variable-info
				  :name symbol
                                  :cst var-name-cst)
	       (continue ()
		 :report (lambda (stream)
			   (format stream "Consider the variable as special."))
                 (setf info
                       (make-instance 'cleavir-env:special-variable-info
                         :name symbol)))
               ;; This is identical to CONTINUE, but more specifically named.
	       (consider-special ()
		 :report (lambda (stream)
			   (format stream "Consider the variable as special."))
                 (setf info
                       (make-instance 'cleavir-env:special-variable-info
                         :name symbol)))
	       (substitute (new-symbol)
		 :report (lambda (stream)
			   (format stream "Substitute a different name."))
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read *query-io*)))
		 (setq info (cleavir-env:variable-info env new-symbol)))))
    info))

(defun function-info (environment function-name-cst)
  (let* ((function-name (cst:raw function-name-cst))
         (result (cleavir-env:function-info environment function-name)))
    (loop while (null result)
	  do (restart-case (error 'no-function-info
				  :name function-name
                                  :cst function-name-cst)
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
	  do (restart-case (error 'no-tag-info
				  :name tag-name
                                  :cst tag-name-cst)
	       (substitute (new-tag-name)
		 :report (lambda (stream)
			   (format stream "Substitute a different name."))
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read *query-io*)))
		 (setq result (cleavir-env:tag-info environment new-tag-name)))))
    result))

(defun block-info (environment block-name-cst)
  (let* ((block-name (cst:raw block-name-cst))
         (info (cleavir-env:block-info environment block-name)))
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
                 (setq info (cleavir-env:block-info environment new-block-name)))))
    info))
