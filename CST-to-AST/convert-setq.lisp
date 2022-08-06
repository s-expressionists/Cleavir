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
  (ast:make-setq-ast (env:identity info)
                     (type-wrap
                      (convert form-cst env system)
                      (env:type info) :setq var-cst env system)
                     :origin var-cst))

(defmethod convert-setq
    (var-cst form-cst (info env:symbol-macro-info) env system)
  (let* ((expansion (env:expansion info))
         (expander (symbol-macro-expander expansion))
         (expanded-variable (expand-macro expander var-cst env))
         (expanded-cst (cst:reconstruct expanded-variable var-cst system
                                        :default-source var-cst))
         (origin (cst:source var-cst)))
    (convert (cst:quasiquote origin
                             (setf (cst:unquote expanded-cst)
                                   ;; FIXME: wrap declared type
                                   (cst:unquote form-cst)))
             env system)))

(defmethod convert-setq-special-variable
    (var-cst form-ast info global-env system)
  (declare (ignore global-env system))
  (let ((temp (ast:make-lexical-variable (gensym) :origin var-cst)))
    (process-progn
     (list (ast:make-lexical-bind-ast temp form-ast :origin var-cst)
           (ast:make-set-constant-symbol-value-ast
            (env:name info)
	    (ast:make-lexical-ast temp :origin var-cst)
	    :origin var-cst)
           (ast:make-lexical-ast temp :origin var-cst))
     var-cst)))

(defmethod convert-setq
    (var-cst form-cst (info env:special-variable-info) env system)
  (let ((global-env (env:global-environment env)))
    (convert-setq-special-variable var-cst
                                   (type-wrap
                                    (convert form-cst env system)
                                    (env:type info) :setq var-cst env system)
				   info
				   global-env
				   system)))

(defun convert-elementary-setq (var-cst form-cst env system)
  (convert-setq var-cst form-cst (variable-info system env var-cst) env system))
