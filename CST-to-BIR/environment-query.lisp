(cl:in-package #:cleavir-cst-to-bir)

(defun variable-info (system environment var-name-cst)
  (let* ((symbol (cst:raw var-name-cst))
         (info (env:variable-info system environment symbol)))
    (loop while (null info)
	  do (restart-case (error 'no-variable-info
				  :name symbol :cst var-name-cst)
	       (continue ()
		 :report "Consider the variable as special."
                 (setf info
                       (make-instance 'env:special-variable-info
                         :name symbol :type (ctype:top system))))
               ;; This is identical to CONTINUE, but more specifically named.
	       (consider-special ()
		 :report "Consider the variable as special."
                 (setf info
                       (make-instance 'env:special-variable-info
                         :name symbol :type (ctype:top system))))
	       (substitute (new-symbol)
		 :report "Substitute a different name."
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read *query-io*)))
		 (setq info (env:variable-info
                             system environment new-symbol)))))
    info))

(defun function-info (system environment function-name-cst)
  (let* ((function-name (cst:raw function-name-cst))
         (result (env:function-info system environment function-name)))
    (loop while (null result)
	  do (restart-case (error 'no-function-info
				  :name function-name :cst function-name-cst)
	       (consider-global ()
		 :report "Treat it as the name of a global function."
		 (return-from function-info
		   (make-instance 'env:global-function-info
		     :name function-name
                     :type (ctype:function-top system))))
	       (substitute (new-function-name)
		 :report "Substitute a different name."
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read *query-io*)))
		 (setq result (env:function-info
                               system environment new-function-name)))))
    result))

(defun tag-info (environment tag-name-cst)
  (let* ((tag-name (cst:raw tag-name-cst))
         (result (env:tag-info environment tag-name)))
    (loop while (null result)
	  do (restart-case (error 'no-tag-info
				  :name tag-name :cst tag-name-cst)
	       (substitute (new-tag-name)
		 :report "Substitute a different name."
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read *query-io*)))
		 (setq result (env:tag-info environment new-tag-name)))))
    result))

(defun block-info (environment block-name-cst)
  (let* ((block-name (cst:raw block-name-cst))
         (info (env:block-info environment block-name)))
    (loop while (null info)
          do (restart-case (error 'no-block-info
                                  :name block-name :cst block-name-cst)
               (substitute (new-block-name)
                 :report "Substitute a different name."
                 :interactive (lambda ()
                                (format *query-io* "Enter new name: ")
                                (list (read *query-io*)))
                 (setq info (env:block-info environment new-block-name)))))
    info))
