(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.

(defmethod convert-setq
    (var-cst form-cst (info trucler:constant-variable-description) env system)
  (declare (ignore var-cst env system))
  (error 'setq-constant-variable :cst form-cst))

(defmethod convert-setq
    (var-cst form-cst (info trucler:lexical-variable-description) env system)
  (let ((origin (cst:source var-cst)))
    (cleavir-ast:make-setq-ast (trucler:identity info)
                               (convert form-cst env system)
                               :origin origin)))

(defmethod convert-setq
    (var-cst form-cst (info trucler:symbol-macro-description) env system)
  (let* ((expansion (trucler:expansion info))
         (expander (symbol-macro-expander expansion))
         (expanded-variable (expand-macro expander var-cst env))
         (expanded-cst (cst:reconstruct expanded-variable var-cst system))
         (origin (cst:source var-cst)))
    (convert (cst:cons (make-atom-cst 'setf origin)
                       (cst:list expanded-cst form-cst)
                       :source origin)
             env system)))

(defmethod convert-setq-special-variable
    (var-cst form-ast info global-env system)
  (declare (ignore global-env system))
  (let* ((origin (cst:source var-cst))
         (temp (cleavir-ast:make-lexical-variable (gensym) :origin origin)))
    (process-progn
     (list (cleavir-ast:make-lexical-bind-ast temp form-ast :origin origin)
	   (cleavir-ast:make-set-symbol-value-ast
	    (cleavir-ast:make-constant-ast (trucler:name info)
              :origin origin)
	    (cleavir-ast:make-lexical-ast temp :origin origin)
	    :origin origin)
           (cleavir-ast:make-lexical-ast temp :origin origin))
     origin)))

(defmethod convert-setq
    (var-cst form-cst (info trucler:special-variable-description) env system)
  (let ((global-env (trucler:global-environment system env)))
    (convert-setq-special-variable var-cst
                                   (convert form-cst env system)
				   info
				   global-env
				   system)))

(defun convert-elementary-setq (var-cst form-cst env system)
  (convert-setq var-cst form-cst (variable-info env var-cst) env system))
