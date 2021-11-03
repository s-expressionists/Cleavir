(in-package #:cleavir-cst-to-bir)

;;; Check the syntax of a single LET or LET* binding.  If the syntax
;;; is incorrect, signal an error.
(defun check-binding (cst)
  (cond ((or (and (cst:atom cst)
                  (symbolp (cst:raw cst)))
             (and (cst:consp cst)
                  (cst:atom (cst:first cst))
                  (symbolp (cst:raw (cst:first cst)))
                  (or (cst:null (cst:rest cst))
                      (and (cst:consp (cst:rest cst))
                           (cst:null (cst:rest (cst:rest cst)))))))
         nil)
        ((cst:atom cst)
         (error 'binding-must-be-symbol-or-list :cst cst))
        ((or (and (cst:atom (cst:rest cst))
                  (not (cst:null (cst:rest cst))))
             (not (cst:null (cst:rest (cst:rest cst)))))
         (error 'binding-must-have-length-one-or-two :cst cst))
        (t
         (error 'variable-must-be-a-symbol :cst (cst:first cst)))))

;;; Check the syntax of the bindings of a LET or a LET* form.  If the
;;; syntax is incorrect, signal an error and propose a restart for
;;; fixing it up.
(defun check-bindings (cst operator)
  (check-cst-proper-list cst 'bindings-must-be-proper-list
                         :operator operator)
  (loop for remaining = cst then (cst:rest remaining)
        until (cst:null remaining)
        do (check-binding (cst:first remaining))))

;;; We convert a LET form CST by binding every variable.
(defmethod convert-let (cst inserter environment system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (let-cst bindings-cst . body-forms-cst) cst
    (declare (ignore let-cst))
    (check-bindings bindings-cst 'let)
    (multiple-value-bind (declaration-csts forms-cst)
        (cst:separate-ordinary-body body-forms-cst)
      (let ((old-dynenv (dynamic-environment inserter))
            (canonical-declaration-specifiers
              (cst:canonicalize-declarations
               system (env:declarations environment) declaration-csts))
            (variable-csts (loop for remaining = bindings-cst
                                   then (cst:rest remaining)
                                 until (cst:null remaining)
                                 collect (let ((binding-cst
                                                 (cst:first remaining)))
                                           (if (cst:atom binding-cst)
                                               binding-cst
                                               (cst:first binding-cst)))))
            ;; Convert the initforms.
            (initform-data
              (loop for remaining = bindings-cst
                      then (cst:rest remaining)
                    until (cst:null remaining)
                    collect (let* ((binding-cst
                                     (cst:first remaining))
                                   (form-cst
                                     (if (or (cst:atom binding-cst)
                                             (cst:null (cst:rest binding-cst)))
                                         (make-atom-cst nil origin)
                                         (cst:second binding-cst)))
                                   (rv
                                     (convert form-cst inserter
                                              environment system)))
                              ;; If an initform never returns,
                              ;; we need not continue
                              (when (eq rv :no-return)
                                (return-from convert-let :no-return))
                              rv)))
            new-env)
        (multiple-value-bind (item-specific-dspecs remaining-dspecs)
            (itemize-declaration-specifiers (mapcar #'list variable-csts)
                                            canonical-declaration-specifiers)
          ;; Set up the new environment.
          ;; AUGMENT-ENVIRONMENT-WITH-VARIABLE also makes the BIR
          ;; variables in the lexical case.
          (loop for variable-cst in variable-csts
                for idspec in item-specific-dspecs
                do (setf new-env
                         (augment-environment-with-variable
                          variable-cst (first idspec)
                          system environment environment)))
          ;; Generate bindings.
          (loop for variable-cst in variable-csts
                for info = (env:variable-info system new-env
                                              (cst:raw variable-cst))
                for initvals in initform-data
                do (bind-variable variable-cst info initvals
                                  inserter system))
          ;; Convert the body.
          (let* ((final-env
                   (augment-environment-with-declarations new-env system
                                                          remaining-dspecs))
                 (rv (convert-progn forms-cst inserter final-env system)))
            ;; Finally, if the inserter is now in a different dynenv
            ;; as would happen for example from a special variable binding,
            ;; return to the previous dynamic environment.
            ;; We also don't need to do this if the body never returns.
            (unless (or (eq rv :no-return)
                        (eq old-dynenv (dynamic-environment inserter)))
              (let ((after (make-iblock inserter
                                        :name '#:let-after
                                        :dynamic-environment old-dynenv)))
                (terminate inserter (make-instance 'cleavir-bir:jump
                                      :inputs () :outputs ()
                                      :next (list after)))
                (begin inserter after)))
            rv))))))

;;; We convert a LET* form CST by transforming it into nested LET form
;;; CSTs and then converting those instead.  This is not trivial,
;;; because we need to associate the right declarations with the
;;; corresponding LET form CST.

(defmethod convert-let* (cst inserter environment system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (let*-cst bindings-cst . body-forms-cst) cst
    (declare (ignore let*-cst))
    (check-bindings bindings-cst 'let*)
    (multiple-value-bind (declaration-csts forms-cst)
        (cst:separate-ordinary-body body-forms-cst)
      (let* ((canonical-declaration-specifiers
               (cst:canonicalize-declarations
                system (env:declarations environment) declaration-csts))
             (binding-csts (cst:listify bindings-cst))
             (variable-csts
               (loop for binding-cst in binding-csts
                     collect (if (cst:atom binding-cst)
                                 binding-cst
                                 (cst:first binding-cst)))))
        (multiple-value-bind (item-specific-dspecs remaining-dspecs)
            (itemize-declaration-specifiers (mapcar #'list variable-csts)
                                            canonical-declaration-specifiers)
          (loop with result = (cst:quasiquote
                               origin
                               (locally
                                   (declare (cst:unquote-splicing
                                             remaining-dspecs))
                                 (cst:unquote-splicing forms-cst)))
                for binding-cst in (reverse binding-csts)
                for declaration-cst-groups in (reverse item-specific-dspecs)
                for declaration-csts = (first declaration-cst-groups)
                for declarations-cst
                  = (cst:cstify declaration-csts :source origin)
                do (setf result (cst:quasiquote
                                 origin
                                 (let ((cst:unquote binding-cst))
                                   (declare
                                    (cst:unquote-splicing declarations-cst))
                                   (cst:unquote result))))
                finally (return
                          (convert result
                                   inserter environment system))))))))
