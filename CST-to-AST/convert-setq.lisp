(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.
;;;
;;; Recall that the SETQ-AST is a NO-VALUE-AST-MIXIN.  We must
;;; therefore make sure it is always compiled in a context where its
;;; value is not needed.  We do that by wrapping a PROGN around it.

(defmethod convert-setq
    (var-cst form-cst (info cleavir-env:constant-variable-info) env system)
  (declare (ignore var-cst form-cst env system))
  (error 'setq-constant-variable
	 :expr (cleavir-env:name info)))

(defmethod convert-setq
    (var-cst form-cst (info cleavir-env:lexical-variable-info) env system)
  (let ((origin (cst:source var-cst)))
    (process-progn
     (list (cleavir-ast:make-setq-ast
            (cleavir-env:identity info)
            (convert form-cst env system)
            :origin origin)
           (cleavir-env:identity info))
     origin)))

(defmethod convert-setq
    (var-cst form-cst (info cleavir-env:symbol-macro-info) env system)
  (let* ((expansion (funcall (coerce *macroexpand-hook* 'function)
                             (lambda (form env)
                               (declare (ignore form env))
                               (cleavir-env:expansion info))
                             (cleavir-env:name info)
                             env))
         (expansion-cst (cst:reconstruct expansion var-cst system))
         (origin (cst:source var-cst)))
    (convert (cst:cons (make-atom-cst 'setf origin)
                       (cst:list expansion-cst form-cst)
                       :source origin)
             env system)))

(defmethod convert-setq-special-variable
    (var-cst form-ast info global-env system)
  (declare (ignore system))
  (let* ((origin (cst:source var-cst))
         (temp (cleavir-ast:make-lexical-ast (gensym) :origin origin)))
    (process-progn
     (list (cleavir-ast:make-setq-ast temp form-ast :origin origin)
	   (cleavir-ast:make-set-symbol-value-ast
	    (cleavir-ast:make-load-time-value-ast `',(cleavir-env:name info) t
                                                  :origin origin)
	    temp
	    :origin origin)
	   temp)
     origin)))

(defmethod convert-setq
    (var-cst form-cst (info cleavir-env:special-variable-info) env system)
  (let ((global-env (cleavir-env:global-environment env)))
    (convert-setq-special-variable var-cst
                                   (convert form-cst env system)
				   info
				   global-env
				   system)))

(defun convert-elementary-setq (var-cst form-cst env system)
  (let* ((symbol (cst:raw var-cst))
         (info (cleavir-env:variable-info env symbol)))
    (loop while (null info)
	  do (restart-case (error 'cleavir-env:no-variable-info
				  :name symbol
				  :origin (cst:source var-cst))
	       (recover ()
		 :report (lambda (stream)
			   (format stream "Consider the variable as special."))
                 (setf info
                       (make-instance 'cleavir-env:special-variable-info
                         :name symbol)))
               ;; This is identical to RECOVER, but more specifically named.
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
    (convert-setq var-cst form-cst info env system)))
