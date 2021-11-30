(in-package #:cleavir-cst-to-bir)

(defun process-non-tlf (cst ctt cproc lproc environment system)
  (when ctt
    (funcall cproc cst environment system))
  (funcall lproc cst environment system)
  (values))

(defgeneric process (cst compile-time-too compile-processor load-processor
                     environment system))

(defgeneric process-cst (cst info compile-time-too
                         compile-processor load-processor
                         environment system))

(defgeneric process-special (head cst compile-time-too
                             compile-processor load-processor
                             environment system))

(defmethod process (cst ctt cproc lproc environment system)
  (let ((form (cst:raw cst)))
    (cond ((and (not (consp form)) (not (symbolp form)))
           (process-non-tlf cst ctt cproc lproc environment system))
          ((symbolp form)
           (let ((info (variable-info system environment cst)))
             (process-cst cst info ctt cproc lproc environment system)))
          ((symbolp (car form))
           (let ((info (function-info system environment (cst:first cst))))
             (process-cst cst info ctt cproc lproc
                          environment system)))
          (t ; lambda form
           (process-non-tlf cst ctt cproc lproc environment system)))))

(defmethod process-cst (cst (info env:special-operator-info)
                        ctt cproc lproc env system)
  (process-special (car (cst:raw cst)) cst ctt cproc lproc env system))

(defmethod process-cst (cst (info env:symbol-macro-info)
                        ctt cproc lproc env system)
  (let* ((expansion (env:expansion info))
         (expander (symbol-macro-expander expansion))
         (expanded-form (expand-macro expander cst env))
         (expanded-cst (cst:reconstruct expanded-form cst system)))
    (process expanded-cst ctt cproc lproc env system)))

(defmethod process-cst (cst (info env:local-macro-info)
                        ctt cproc lproc env system)
  (let* ((expander (env:expander info))
         (expanded-form (expand-macro expander cst env))
         (expanded-cst (cst:reconstruct expanded-form cst system)))
    (process expanded-cst ctt cproc lproc env system)))
;;; See note below about compiler macros.
(defmethod process-cst (cst (info env:global-macro-info)
                        ctt cproc lproc env system)
  (let* ((expander (env:expander info))
         (expanded-form (expand-macro expander cst env))
         (expanded-cst (cst:reconstruct expanded-form cst system)))
    (process expanded-cst ctt cproc lproc env system)))

(defmethod process-cst (cst (info env:constant-variable-info)
                        ctt cproc lproc env system)
  (process-non-tlf cst ctt cproc lproc env system))
(defmethod process-cst (cst (info env:special-variable-info)
                        ctt cproc lproc env system)
  (process-non-tlf cst ctt cproc lproc env system))
(defmethod process-cst (cst (info env:lexical-variable-info)
                        ctt cproc lproc env system)
  (process-non-tlf cst ctt cproc lproc env system))

;;; We could potentially try to expand compiler macros here. However, the
;;; standard does not compel implementations to ever expand compiler macros,
;;; so code relying on compiler macros to expand into toplevel forms is
;;; nonportable. In fact, compiler macros sometimes have to be carefully
;;; written to avoid toplevelness (e.g. (IDENTITY X) cannot be expanded into
;;; X, in case X could be a toplevel form). So we don't expand.
;;; A client could do so if it wanted by specializing this generic function.
(defmethod process-cst (cst (info env:global-function-info)
                        ctt cproc lproc env system)
  (process-non-tlf cst ctt cproc lproc env system))
(defmethod process-cst (cst (info env:local-function-info)
                        ctt cproc lproc env system)
  (process-non-tlf cst ctt cproc lproc env system))

;;; The presence of this default method means that Cleavir will treat all
;;; special forms other than those specialized on here (i.e., other than
;;; PROGN, EVAL-WHEN, MACROLET, SYMBOL-MACROLET, LOCALLY) as not preserving
;;; toplevelness. If a client has other special forms to treat as toplevel,
;;; they need to specialize PROCESS-SPECIAL appropriately.
(defmethod process-special (head cst ctt cproc lproc env system)
  (declare (ignore head))
  (process-non-tlf cst ctt cproc lproc env system))

(defun process-sequence (sequence-cst ctt cproc lproc env system)
  (loop for seq = sequence-cst then (cst:rest seq)
        until (cst:null seq)
        do (process (cst:first seq) ctt cproc lproc env system))
  (values))

(defmethod process-special ((head (eql 'cl:progn)) cst
                            ctt cproc lproc env system)
  (process-sequence (cst:rest cst) ctt cproc lproc env system))

;;; "Evaluate" a sequence of forms in the sense of CLHS 3.2.3.1.
;;; This means we call cproc on them, and cproc does whatever
;;; "evaluation" means in this context.
(defun evaluate-sequence (sequence-cst compile-processor env system)
  (loop for seq = sequence-cst then (cst:rest seq)
        until (cst:null seq)
        do (funcall compile-processor (cst:first seq) env system))
  (values))

(defmethod process-special ((head (eql 'cl:eval-when)) cst
                            ctt cproc lproc env system)
  (check-eval-when-syntax cst)
  (cst:db s (eval-when-cst situations-cst . body-cst) cst
    (declare (ignore eval-when-cst))
    (let* ((situations (cst:raw situations-cst))
           ;; These correspond to the abbreviations in CLHS Figure 3-7.
           (ct (or (member :compile-toplevel situations)
                   (member 'cl:compile situations)))
           (lt (or (member :load-toplevel situations)
                   (member 'cl:load situations)))
           (e  (or (member :execute situations)
                   (member 'cl:eval situations))))
      ;; Process according to Figure 3-7 in
      ;; CLHS 3.2.3.1, "Processing of Top Level Forms".
      (cond (;; Process in compile-time-too mode
             (or
              ;; CT   LT   E    Mode
              ;; Yes  Yes  ---  ---
              ;; No   Yes  Yes  CTT
              (and ct lt)
              (and (not ct) lt e ctt))
             (process-sequence body-cst t cproc lproc env system))
            (;; Process in not-compile-time mode
             (or
              ;; CT   LT   E    Mode
              ;; No   Yes  Yes  NCT
              ;; No   Yes  No   ---
              (and (not ct) lt e (not ctt))
              (and (not ct) lt (not e)))
             (process-sequence body-cst nil cproc lproc env system))
            (;; Evaluate (and don't process)
             (or
              ;; CT   LT   E    Mode
              ;; Yes  No   ---  ---
              ;; No   No   Yes  CTT
              (and ct (not lt))
              (and (not ct) (not lt) e ctt))
             (evaluate-sequence body-cst cproc env system))
            (;; Discard
             ;; CT   LT    E    Mode
             ;; No   No    Yes  NCT
             ;; No   No    No   ---
             ;; (But we've exhausted the cases at this point.)
             t
             (values))))))

(defmethod process-special ((head (eql 'cl:macrolet))
                            cst ctt cproc lproc env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (macrolet-cst definitions-cst . body-cst) cst
    (declare (ignore macrolet-cst))
    (check-function-bindings definitions-cst head)
    (let ((new-env env))
      (loop for remaining = definitions-cst then (cst:rest remaining)
            until (cst:null remaining)
            do (let* ((definition-cst (cst:first remaining))
                      (name-cst (cst:first definition-cst))
                      (name (cst:raw name-cst))
                      (expander (expander definition-cst env system)))
                 (setf new-env
                       (env:add-local-macro new-env name expander))))
      (process (cst:quasiquote origin
                               (locally (cst:unquote-splicing body-cst)))
               ctt cproc lproc new-env system))))

(defmethod process-special
    ((head (eql 'cl:symbol-macrolet)) cst ctt cproc lproc env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (symbol-macrolet-cst definitions-cst . body-cst) cst
    (declare (ignore symbol-macrolet-cst))
    (let ((new-env env))
      (loop for remaining = definitions-cst then (cst:rest remaining)
            until (cst:null remaining)
            do (cst:db definition-origin (name-cst expansion-cst)
                   (cst:first remaining)
                 (let* ((name (cst:raw name-cst))
                        ;; We use cleavir-env directly, because it's
                        ;; okay if the variable is unbound.
                        (info (env:variable-info system env name))
                        (expansion (cst:raw expansion-cst)))
                   (typecase info
                     ;; NOTE: We are at toplevel, so special and lexical
                     ;; variables are not possible.
                     (env:constant-variable-info
                      (cerror "Bind it anyway."
                              'symbol-macro-names-constant :cst name-cst)))
                   (setf new-env
                         (env:add-local-symbol-macro
                          new-env name expansion)))))
      (process (cst:quasiquote origin
                               (locally (cst:unquote-splicing body-cst)))
               ctt cproc lproc new-env system))))

(defmethod process-special
    ((symbol (eql 'cl:locally)) cst ctt cproc lproc environment system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (cst:db origin (locally-cst . body-forms-cst) cst
    (declare (ignore locally-cst))
    (multiple-value-bind (declaration-csts forms-cst)
        (cst:separate-ordinary-body body-forms-cst)
      (let* ((canonical-declaration-specifiers
               (cst:canonicalize-declarations
                system (env:declarations environment) declaration-csts))
             (new-env (augment-environment-with-declarations
                       environment system canonical-declaration-specifiers)))
        (process-sequence forms-cst ctt cproc lproc new-env system)))))
