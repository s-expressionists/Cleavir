(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a symbol macro.

(defmethod convert-cst
    (client cst (description trucler:symbol-macro-description) env)
  (let* ((expansion (trucler:expansion description))
         (expander (symbol-macro-expander expansion))
         (expanded-form (expand-macro expander cst env))
         (expanded-cst (cst:reconstruct client expanded-form cst
                                        :default-source cst)))
    (with-preserved-toplevel-ness
      (convert client expanded-cst env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a constant variable.

(defmethod convert-cst
    (client cst (description trucler:constant-variable-description) env)
  (let ((cst (cst:cst-from-expression (trucler:value description)
                                      :source (cst:source cst))))
    (convert-constant client cst env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a special form represented as a CST.

(defmethod convert-cst
    (client cst (description trucler:special-operator-description) env)
  (convert-special client (car (cst:raw cst)) cst env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form that calls a local macro.
;;; A local macro can not have a compiler macro associated with it.
;;;
;;; If we found a local macro in ENV, it means that ENV is not the
;;; global environment.  And it must be the same kind of agumentation
;;; environment that was used when the local macro was created by the
;;; use of MACROLET.  Therefore, the expander should be able to handle
;;; being passed the same kind of environment.

(defmethod convert-cst
    (client cst (description trucler:local-macro-description) env)
  (let* ((expander (trucler:expander description))
         (expanded-form (expand-macro expander cst env))
         (expanded-cst (cst:reconstruct client expanded-form cst
                                        :default-source cst)))
    (with-preserved-toplevel-ness
      (convert client expanded-cst env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form that calls a global macro.
;;; A global macro can have a compiler macro associated with it.

(defmethod convert-cst
    (client cst (description trucler:global-macro-description) env)
  (let ((compiler-macro (trucler:compiler-macro description))
        (notinline (eq 'notinline (trucler:inline description)))
        (expander (trucler:expander description)))
    (with-preserved-toplevel-ness
      (if (or notinline (null compiler-macro))
          ;; There is no compiler macro, or its use has been disabled,
          ;; so we just apply the macro expander, and then convert
          ;; the resulting form.
          (let* ((expanded-form (expand-macro expander cst env))
                 (expanded-cst (cst:reconstruct client expanded-form cst
                                                :default-source cst)))
            (convert client expanded-cst env))
          ;; There is a compiler macro, so we must see whether it will
          ;; accept or decline.
          (let ((expanded-form (expand-compiler-macro compiler-macro cst env)))
            (if (eq (cst:raw cst) expanded-form)
                ;; If the two are EQ, this means that the compiler macro
                ;; declined.  Then we appply the macro function, and
                ;; then convert the resulting form, just like we did
                ;; when there was no compiler macro present.
                (let* ((expanded-form
                         (expand-macro expander cst env))
                       (expanded-cst (cst:reconstruct client expanded-form cst
                                                      :default-source cst)))
                  (convert client expanded-cst env))
                ;; If the two are not EQ, this means that the compiler
                ;; macro replaced the original form with a new form.
                ;; This new form must then again be converted without
                ;; taking into account the real macro expander.
                (let ((expanded-cst (cst:reconstruct client expanded-form cst
                                                     :default-source cst)))
                  (convert client expanded-cst env))))))))

;;; Construct a CALL-AST representing a function-call form.  CST is
;;; the concrete syntax tree representing the entire function-call
;;; form.  ARGUMENTS-CST is a CST representing the sequence of
;;; arguments to the call.
(defun make-call (client cst description env arguments-cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (let* ((name-cst (cst:first cst))
         (function-ast (convert-called-function-reference
                        client name-cst description env))
         (argument-asts (convert-sequence client arguments-cst env))
         (ftype (trucler:type description)))
    (let ((required (ctype:function-required client ftype))
          (optional (ctype:function-optional client ftype))
          (rest (ctype:function-rest client ftype))
          (keysp (ctype:function-keysp client ftype))
          (values (ctype:function-values client ftype)))
      (type-wrap
       client
       (ast:make-call-ast function-ast
                          (mapcar
                           (lambda (argument-ast)
                             (type-wrap
                              client
                              argument-ast
                              (cond (required (pop required))
                                    (optional (pop optional))
                                    ;; FIXME: Actually treat &key properly!
                                    (keysp (ctype:top client))
                                    (t (if (ctype:bottom-p client rest)
                                           (progn
                                             ;; FIXME: Use a
                                             ;; condition
                                             ;; class here.
                                             (warn "A call to ~a was passed a number of arguments incompatible with its declared type ~a."
                                                   (cst:raw name-cst) ftype)
                                             ;; Without this
                                             ;; we'll get a
                                             ;; borked call
                                             ;; as a result.
                                             (ctype:top client))
                                           rest)))
                              :argument cst env))
                           argument-asts)
                          :origin cst
                          :inline (trucler:inline description))
       values :return cst env))))

;;; Convert a form representing a call to a named global function.
;;; CST is the concrete syntax tree representing the entire
;;; function-call form.  DESCRIPTION is the description instance
;;; returned from a query of the environment with the name of the
;;; function.
(defmethod convert-cst
    (client cst (description trucler:global-function-description) env)
  ;; When we compile a call to a global function, it is possible that
  ;; we are in COMPILE-TIME-TOO mode.  In that case, we must first
  ;; evaluate the form.
  (when (and *current-form-is-top-level-p* *compile-time-too*)
    (cst-eval-for-effect-encapsulated client cst env))
  (let ((compiler-macro (trucler:compiler-macro description))
        (notinline (eq 'notinline (trucler:inline description))))
    (if (or notinline (null compiler-macro))
        ;; There is no compiler macro.  Create the call.
        (make-call client cst description env (cst:rest cst))
        ;; There is a compiler macro.  We must see whether it will
        ;; accept or decline.
        (let ((expanded-form (expand-compiler-macro compiler-macro cst env)))
          (if (eq (cst:raw cst) expanded-form)
              ;; If the two are EQ, this means that the compiler macro
              ;; declined.  We are left with function-call form.
              ;; Create the call, just as if there were no compiler
              ;; macro present.
              (make-call client cst description env (cst:rest cst))
              ;; If the two are not EQ, this means that the compiler
              ;; macro replaced the original form with a new form.
              ;; This new form must then be converted.
              (let ((expanded-cst (cst:reconstruct client expanded-form cst
                                                   :default-source cst)))
                (convert client expanded-cst env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a CST representing a compound form that calls a local
;;; function.  A local function can not have a compiler macro
;;; associated with it.

(defmethod convert-cst
    (client cst (description trucler:local-function-description) env)
  (make-call client cst description env (cst:rest cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a special variable.
;;; We do this by generating a call to SYMBOL-VALUE.

(defmethod convert-special-variable (client cst description global-env)
  (declare (ignore client global-env))
  (ast:make-constant-symbol-value-ast (trucler:name description) :origin cst))

(defmethod convert-cst
    (client cst (description trucler:special-variable-description) env)
  (let ((global-env (trucler:global-environment client env)))
    (type-wrap client
               (convert-special-variable client cst description global-env)
               (trucler:type description) :variable cst env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a lexical variable.

(defmethod convert-cst
    (client cst (description trucler:lexical-variable-description) env)
  (when (eq (trucler:ignore description) 'ignore)
    (warn 'ignored-variable-referenced :cst cst))
  (type-wrap client
             (ast:make-lexical-ast (trucler:identity description) :origin cst)
             (trucler:type description) :variable cst env))
