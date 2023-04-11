(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.

(defmethod convert-setq
    (client var-cst form-cst
     (description trucler:constant-variable-description)
     env)
  (declare (ignore client var-cst env))
  (error 'setq-constant-variable :cst form-cst))

(defmethod convert-setq
    (client var-cst form-cst
     (description trucler:lexical-variable-description)
     env)
  (ast:make-setq-ast (trucler:identity description)
                     (type-wrap
                      client
                      (convert client form-cst env)
                      (trucler:type description)
                      :setq var-cst env)
                     :origin var-cst))

(defmethod convert-setq
    (client var-cst form-cst
     (description trucler:symbol-macro-description)
     env)
  (let* ((expansion (trucler:expansion description))
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
    (client var-cst form-ast description global-env)
  (declare (ignore client global-env))
  (let ((temp (ast:make-lexical-variable (gensym) :origin var-cst)))
    (process-progn
     (list (ast:make-lexical-bind-ast temp form-ast :origin var-cst)
           (ast:make-set-constant-symbol-value-ast
            (trucler:name description)
            (ast:make-lexical-ast temp :origin var-cst)
            :origin var-cst)
           (ast:make-lexical-ast temp :origin var-cst))
     var-cst)))

(defmethod convert-setq
    (client var-cst form-cst
     (description trucler:special-variable-description)
     env)
  (let ((global-env (trucler:global-environment client env)))
    (convert-setq-special-variable
     client var-cst
     (type-wrap
      client
      (convert client form-cst env)
      (trucler:type description)
      :setq var-cst env)
     description global-env)))

(defun convert-elementary-setq (client var-cst form-cst env)
  (convert-setq client var-cst form-cst
                (describe-variable client env var-cst)
                env))
