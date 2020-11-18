(cl:in-package #:cleavir-cst-to-ast)

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

(defun process-remaining-let-bindings (bindings idspecs rdspecs body-forms-cst environment system)
  (if (null bindings)
      ;; We ran out of bindings.  We must build an AST for the body of
      ;; the function.
      (let ((new-env (augment-environment-with-declarations environment system rdspecs)))
	(process-progn (convert-sequence body-forms-cst new-env system)))
      (destructuring-bind (variable-cst . lexical-ast) (first bindings)
	(let* (;; We enter the new variable into the environment and
	       ;; then we process remaining parameters and ultimately
	       ;; the body of the function.
	       (new-env (augment-environment-with-variable
			 variable-cst (first (first idspecs)) system environment environment))
	       ;; We compute the AST of the remaining computation by
	       ;; recursively calling this same function with the
	       ;; remaining bindings (if any) and the environment that
	       ;; we obtained by augmenting the original one with the
	       ;; parameter variable.
	       (next-ast (process-remaining-let-bindings (rest bindings)
							 (rest idspecs)
							 rdspecs
							 body-forms-cst
							 new-env
							 system)))
	  ;; All that is left to do now, is to construct the AST to
	  ;; return by using the new variable and the AST of the
	  ;; remaining computation as components.
	  (set-or-bind-variable variable-cst lexical-ast next-ast new-env system)))))

;;; We convert a LET form CST by lexically binding every variable.
(defmethod convert-let (cst environment system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (let-cst bindings-cst . body-forms-cst) cst
    (declare (ignore let-cst))
    (check-bindings bindings-cst 'let)
    (multiple-value-bind (declaration-csts forms-cst)
        (cst:separate-ordinary-body body-forms-cst)
      (let* ((canonical-declaration-specifiers
               (cst:canonicalize-declarations
                system (cleavir-env:declarations environment) declaration-csts))
             (binding-csts (cst:listify bindings-cst))
             (variable-csts (loop for binding-cst in binding-csts
                                  collect (if (cst:atom binding-cst)
                                              binding-cst
                                              (cst:first binding-cst))))
             (initform-csts (loop for binding-cst in binding-csts
                                  collect (if (or (cst:atom binding-cst)
                                                  (cst:null (cst:rest binding-cst)))
                                              (make-atom-cst nil origin)
                                              (cst:second binding-cst))))
             (value-asts (loop for initform-cst in initform-csts
                               collect (convert initform-cst environment system))))
        (multiple-value-bind (item-specific-dspecs remaining-dspecs)
            (itemize-declaration-specifiers (mapcar #'list variable-csts)
                                            canonical-declaration-specifiers)
          (process-remaining-let-bindings (mapcar #'cons variable-csts value-asts)
                                          item-specific-dspecs
                                          remaining-dspecs
                                          forms-cst
                                          environment
                                          system))))))

;;; NOTE: The following is how LETs used to be converted. We needed to
;;; go through a LAMBDA form for technical reasons related to cell
;;; conversion in HIR, but in BIR this reason no longer exists, so we
;;; convert directly to lexical bind instructions for efficiency
;;; reasons.

;;; We convert a LET form CST by transforming it into an equivalent
;;; LAMBDA form CST.
#+(or)
(defmethod convert-let (cst environment system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (let-cst bindings-cst . body-forms-cst) cst
    (declare (ignore let-cst))
    (check-bindings bindings-cst 'let)
    (let* ((binding-csts (cst:listify bindings-cst))
           (variable-csts (loop for binding-cst in binding-csts
                                collect (if (cst:atom binding-cst)
                                            binding-cst
                                            (cst:first binding-cst))))
           (initform-csts (loop for binding-cst in binding-csts
                                collect (if (or (cst:atom binding-cst)
                                                (cst:null (cst:rest binding-cst)))
                                            (make-atom-cst nil origin)
                                            (cst:second binding-cst))))
           (lambda-form-cst
             (make-instance 'cst:cons-cst
               :raw `((lambda ,(mapcar #'cst:raw variable-csts)
                        ,@(cst:raw body-forms-cst))
                      ,(mapcar #'cst:raw initform-csts))
               :source origin
               :first (cst:cons (make-atom-cst 'lambda origin)
                                (cst:cons (cst:cstify variable-csts)
                                          body-forms-cst)
                                :source origin)
               :rest (cst:cstify initform-csts))))
      (convert lambda-form-cst environment system))))

;;; We convert a LET* form CST by transforming it into nested LET form
;;; CSTs and then converting those instead.  This is not trivial,
;;; because we need to associate the right declarations with the
;;; corresponding LET form CST.

(defmethod convert-let* (cst environment system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (let*-cst bindings-cst . body-forms-cst) cst
    (declare (ignore let*-cst))
    (check-bindings bindings-cst 'let*)
    (multiple-value-bind (declaration-csts forms-cst)
        (cst:separate-ordinary-body body-forms-cst)
      (let* ((canonical-declaration-specifiers
               (cst:canonicalize-declarations
                system (cleavir-env:declarations environment) declaration-csts))
             (binding-csts (cst:listify bindings-cst))
             (variable-csts
               (loop for binding-cst in binding-csts
                     collect (if (cst:atom binding-cst)
                                 binding-cst
                                 (cst:first binding-cst)))))
        (multiple-value-bind (item-specific-dspecs remaining-dspecs)
            (itemize-declaration-specifiers (mapcar #'list variable-csts)
                                            canonical-declaration-specifiers)
          (loop with remaining-dspecs-cst = (cst:cstify remaining-dspecs)
                with result = (cst:cons (make-atom-cst 'locally origin)
                                        (if (null remaining-dspecs)
                                            forms-cst
                                            (cst:cons
                                             (cst:cons (make-atom-cst 'declare origin)
                                                       remaining-dspecs-cst
                                                       :source (cst:source remaining-dspecs-cst))
                                             forms-cst
                                             :source origin))
                                        :source origin)
                for binding-cst in (reverse binding-csts)
                for declaration-cst-groups in (reverse item-specific-dspecs)
                for declaration-csts = (first declaration-cst-groups)
                do (setf result
                         (cst:cons
                          (make-atom-cst 'let origin)
                          (cst:cons (cst:list binding-cst)
                                    (if (null declaration-csts)
                                        (cst:list result)
                                        (cst:list
                                         (cst:cons (make-atom-cst 'declare)
                                                   (cst:cstify declaration-csts))
                                         result))
                                    :source origin)
                          :source origin))
                finally (return (convert result environment system))))))))
