(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.

(defmethod convert-setq
    (var-cst form-cst (info env:constant-variable-info) env system)
  (declare (ignore var-cst env system))
  (error 'setq-constant-variable :cst form-cst))

(defmethod convert-setq
    (var-cst form-cst (info env:lexical-variable-info) env system)
  (let ((origin (cst:source var-cst)))
    (ast:make-setq-ast (env:identity info)
                       (convert form-cst env system)
                       :origin origin)))

(defmethod convert-setq
    (var-cst form-cst (info env:symbol-macro-info) env system)
  (let* ((expansion (env:expansion info))
         (expander (symbol-macro-expander expansion))
         (expanded-variable (expand-macro expander var-cst env))
         (expanded-cst (cst:reconstruct expanded-variable var-cst system))
         (origin (cst:source var-cst)))
    (convert (cst:quasiquote origin
                             (setf (cst:unquote expanded-cst)
                                   (cst:unquote form-cst)))
             env system)))

(defmethod convert-setq-special-variable
    (var-cst form-ast info global-env system)
  (declare (ignore global-env system))
  (let* ((origin (cst:source var-cst))
         (temp (ast:make-lexical-variable (gensym) :origin origin)))
    (process-progn
     (list (ast:make-lexical-bind-ast temp form-ast :origin origin)
	   (ast:make-set-symbol-value-ast
	    (ast:make-constant-ast (env:name info)
              :origin origin)
	    (ast:make-lexical-ast temp :origin origin)
	    :origin origin)
           (ast:make-lexical-ast temp :origin origin))
     origin)))

(defmethod convert-setq
    (var-cst form-cst (info env:special-variable-info) env system)
  (let ((global-env (env:global-environment env)))
    (convert-setq-special-variable var-cst
                                   (convert form-cst env system)
				   info
				   global-env
				   system)))

(defun convert-elementary-setq (var-cst form-cst env system)
  (convert-setq var-cst form-cst (variable-info system env var-cst) env system))
