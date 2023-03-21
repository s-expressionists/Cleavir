(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.

(defmethod convert-setq
    (client var-cst form-cst (info env:constant-variable-info) env)
  (declare (ignore client var-cst env))
  (error 'setq-constant-variable :cst form-cst))

(defmethod convert-setq
    (client var-cst form-cst (info env:lexical-variable-info) env)
  (ast:make-setq-ast (env:identity info)
                     (type-wrap
                      client
                      (convert client form-cst env)
                      (env:type info) :setq var-cst env)
                     :origin var-cst))

(defmethod convert-setq
    (client var-cst form-cst (info env:symbol-macro-info) env)
  (let* ((expansion (env:expansion info))
         (expander (symbol-macro-expander expansion))
         (expanded-variable (expand-macro expander var-cst env))
         (expanded-cst (cst:reconstruct client expanded-variable var-cst
                                        :default-source var-cst))
         (origin (cst:source var-cst)))
    (convert client
             (cst:quasiquote origin
                             (setf (cst:unquote expanded-cst)
                                   ;; FIXME: wrap declared type
                                   (cst:unquote form-cst)))
             env)))

(defmethod convert-setq-special-variable
    (client var-cst form-ast info global-env)
  (declare (ignore client global-env))
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
    (client var-cst form-cst (info env:special-variable-info) env)
  (let ((global-env (env:global-environment env)))
    (convert-setq-special-variable client
                                   var-cst
                                   (type-wrap
                                    client
                                    (convert client form-cst env)
                                    (env:type info) :setq var-cst env)
                                   info
                                   global-env)))

(defun convert-elementary-setq (client var-cst form-cst env)
  (convert-setq client var-cst form-cst (variable-info client env var-cst) env))
