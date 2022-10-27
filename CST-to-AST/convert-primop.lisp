(cl:in-package #:cleavir-cst-to-ast)

(defun check-simple-primop-syntax (cst argument-count)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst argument-count argument-count))

(defun convert-primop (symbol cst env system)
  (let* ((info (cleavir-primop-info:info symbol))
         (ninputs (cleavir-primop-info:ninputs info)))
    (check-simple-primop-syntax cst ninputs)
    (cst:db origin (op-cst . args-cst) cst
      (declare (ignore op-cst))
      (make-instance 'ast:primop-ast
        :info info
        :argument-asts (convert-sequence args-cst env system)
        :attributes (cleavir-primop-info:attributes info)
        :origin cst))))

(defmacro defprimop (symbol)
  `(defmethod convert-special
       ((symbol (eql ',symbol)) cst env system)
     (convert-primop symbol cst env system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:AST.
;;;
;;; This allows ASTs produced by other means to be inserted into
;;; code which is then converted again.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:ast)) cst env system)
  (declare (ignore env system))
  (check-simple-primop-syntax cst 1)
  (cst:db origin (primop-cst ast-cst) cst
    (declare (ignore primop-cst))
    (cst:raw ast-cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:EQ.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:eq)) cst env system)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (eq-cst arg1-cst arg2-cst) cst
    (declare (ignore eq-cst))
    (ast:make-eq-ast
     (convert arg1-cst env system)
     (convert arg2-cst env system)
     :origin cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:THE.
;;;
;;; This primitive operation represents CL:THE strictly in
;;; the capacity as a declaration, i.e. not an assertion.
;;; Clients may choose to expand CL:THE forms into uses of
;;; this operator in situations where a type check is not
;;; what they want to do.
;;; This operator has the same syntax as CL:THE, except
;;; it is unaffected by client logic in type-wrap etc.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:the)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 2)
  (cst:db origin (the-cst value-type-cst form-cst) cst
    (declare (ignore the-cst))
    (let ((vctype (env:parse-values-type-specifier
                   (cst:raw value-type-cst)
                   env system)))
      (ast:make-the-ast
       (convert form-cst env system)
       vctype
       nil
       :origin cst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:TRULY-THE.
;;;
;;; This primitive operation represents a trusted type
;;; declaration that the compiler should not bother
;;; checking. Use of this may be dangerous.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:truly-the)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 2)
  (cst:db origin (the-cst value-type-cst form-cst) cst
    (declare (ignore the-cst))
    (let ((vctype (env:parse-values-type-specifier
                   (cst:raw value-type-cst)
                   env system)))
      (ast:make-the-ast
       (convert form-cst env system)
       vctype
       :trusted
       :origin cst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:ENSURE-THE.
;;;
;;; This primitive operation represents a type check.
;;; The checking function must be provided.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:ensure-the)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 3 3)
  (cst:db origin (the-cst value-type-cst
                          type-check-function-cst form-cst)
      cst
    (declare (ignore the-cst))
    (let ((vctype (env:parse-values-type-specifier
                   (cst:raw value-type-cst)
                   env system)))
      (ast:make-the-ast
       (convert form-cst env system)
       vctype
       ;; FIXME: Check that it's a lambda expression
       (convert type-check-function-cst env system)
       :origin cst))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:TYPEQ.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:typeq)) cst env system)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (typeq-cst arg1-cst arg2-cst) cst
    (declare (ignore typeq-cst))
    (ast:make-typeq-ast
     (convert arg1-cst env system)
     (env:parse-type-specifier (cst:raw arg2-cst) env system)
     :origin cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:CASE.
;;;
;;; This primitive operation can be used to compile CL:CASE
;;; efficiently. It has the same syntax as CL:CASE, except
;;; that the T/OTHERWISE case is not optional, and the keys
;;; must be actual lists rather than designators thereof.
;;; Note that the keys are passed pretty directly to the
;;; backend past HIR level. Implementations using this
;;; operation should ensure that only keys it is prepared
;;; to compare against (immediates, most likely) are used.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:case)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 nil) ; keyform and default case
  (cst:db origin (case-cst keyform-cst . case-csts) cst
    (declare (ignore case-cst))
    (let* ((all-cases (cst:listify case-csts))
           (cases (butlast all-cases))
           (default (first (last all-cases))))
      (check-cst-proper-list default 'case-must-be-proper-list)
      (unless (member (cst:raw (cst:first default)) '(t otherwise))
        (error 'default-case-missing :cst cst))
      (loop for case in cases
            ;; FIXME: Also not actually forms.
            do (check-cst-proper-list case 'case-must-be-proper-list)
               (check-cst-proper-list (cst:first case)
                                      'case-keys-must-be-proper-list)
            collect (cst:raw (cst:first case)) into comparees
            collect (cst:rest case) into dests
            finally (return
                      (ast:make-branch-ast
                       (ast:make-case-ast
                        (convert keyform-cst env system)
                        comparees
                        :origin cst)
                       (loop for body in dests
                             collect (process-progn
                                      (convert-sequence body env system)
                                      cst))
                       (process-progn
                        (convert-sequence (cst:rest default) env system)
                        cst)
                       :origin cst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FUNCALL.
;;;
;;; This primop is similar to the function CL:FUNCALL.  The difference
;;; is that the primop does not allow a function NAME as its first
;;; argument.  It has to be a form that evaluates to a function.
;;;
;;; In order to inline CL:FUNCALL, a possible strategy would be to
;;; define a compiler macro on that function that expands to a form
;;; that turns the first argument into a function if it is not already
;;; a function and then calls this primop.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:funcall)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (funcall-cst function-cst . arguments-cst) cst
    (declare (ignore funcall-cst))
    (ast:make-call-ast
     (convert function-cst env system)
     (loop for remaining = arguments-cst then (cst:rest remaining)
           until (cst:null remaining)
           collect (convert (cst:first remaining) env system))
     :origin cst
     ;; FIXME: propagate inline here somehow.
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:MULTIPLE-VALUE-CALL.
;;;
;;; This primop is similar to the special operator CL:MULTIPLE-VALUE-CALL.
;;; The difference is that the primop does not allow a function NAME
;;; as its first argument.  It has to be a form that evaluates to a
;;; function.
;;;
;;; CL:MULTIPLE-VALUE-CALL can be defined as a macro expanding into
;;; a form that turns the first argument into a function if it is not
;;; already a function and then calls this primop.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:multiple-value-call)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (multiple-value-call-cst function-cst . arguments-cst) cst
    (declare (ignore multiple-value-call-cst))
    (ast:make-multiple-value-call-ast
     (convert function-cst env system)
     (loop for remaining = arguments-cst then (cst:rest remaining)
           until (cst:null remaining)
           collect (convert (cst:first remaining) env system))
     :origin cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:UNREACHABLE.
;;;
;;; Recall that this primop indicates that execution of the form
;;; should be impossible.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:unreachable)) cst env system)
  (declare (ignore env system))
  (check-simple-primop-syntax cst 0)
  (make-instance 'ast:unreachable-ast :origin cst))
