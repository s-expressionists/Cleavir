(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.

(defmethod convert-setq
    (var-cst form-cst (info cleavir-env:constant-variable-info) env system)
  (declare (ignore var-cst env system))
  (error 'setq-constant-variable :cst form-cst))

(defmethod convert-setq
    (var-cst form-cst (info cleavir-env:lexical-variable-info) env system)
  (let ((origin (cst:source var-cst)))
    (cleavir-ast:make-setq-ast (cleavir-env:identity info)
                               (convert form-cst env system)
                               :origin origin)))

(defmethod convert-setq
    (var-cst form-cst (info cleavir-env:symbol-macro-info) env system)
  (let* ((expansion (cleavir-env:expansion info))
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
	    (cleavir-ast:make-constant-ast (cleavir-env:name info)
              :origin origin)
	    (cleavir-ast:make-lexical-ast temp :origin origin)
	    :origin origin)
           (cleavir-ast:make-lexical-ast temp :origin origin))
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
  (convert-setq var-cst form-cst (variable-info env var-cst) env system))
