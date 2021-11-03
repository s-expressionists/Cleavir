(cl:in-package #:cleavir-cst-to-bir)

(defun check-simple-primop-syntax (cst argument-count)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst argument-count argument-count))

(defun convert-primop (symbol cst inserter env system)
  (let* ((info (cleavir-primop-info:info symbol))
         (attributes (cleavir-primop-info:attributes info))
         (ninputs (cleavir-primop-info:ninputs info))
         (out (cleavir-primop-info:out-kind info)))
    (check-simple-primop-syntax cst ninputs)
    (when (integerp out)
      ;; FIXME: use a condition type
      (error "BUG: Test primop in invalid context: ~a" cst))
    (with-compiled-arguments (args (cst:rest cst) inserter env system)
      (let ((outputs (ecase out
                       ((:value) (list (make-instance 'bir:output)))
                       ((:effect) nil))))
        (insert inserter
                (make-instance 'bir:primop
                  :info info :attributes attributes
                  :inputs args :outputs outputs))
        (copy-list outputs)))))

(defmacro defprimop (symbol)
  `(defmethod convert-special
       ((symbol (eql ',symbol)) cst inserter env system)
     (convert-primop symbol cst inserter env system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:AST.
;;;
;;; This allows ASTs produced by other means to be inserted into
;;; code which is then converted again.

#+(or) ; TODO
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
    ((symbol (eql 'cleavir-primop:eq)) cst inserter env system)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (eq-cst arg1-cst arg2-cst) cst
    (declare (ignore eq-cst))
    (with-compiled-csts (args (arg1-cst arg2-cst) inserter env system)
      (let ((out (make-instance 'bir:output)))
        (insert inserter 'cleavir-bir:eq-test
                :inputs args :outputs (list out))
        (list out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:THE.
;;;
;;; This primitive operation represents CL:THE strictly in
;;; the capacity as a declaration, i.e. not an assertion.
;;; Clients may choose to expand CL:THE forms into uses of
;;; this operator in situations where a type check is not
;;; what they want to do.
;;; This operator has the same syntax as CL:THE.

#+(or)
(defmethod convert-special
    ((symbol (eql 'cleavir-primop:the)) cst inserter env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 2)
  (cst:db origin (the-cst value-type-cst form-cst) cst
    (declare (ignore the-cst))
    (with-compiled-cst (values form-cst inserter env system)
      (let ((vctype (env:parse-values-type-specifier
                     (cst:raw value-type-cst)
                     env system)))
      (ast:make-the-ast
       (convert form-cst env system)
       vctype
       nil
       :origin origin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:TYPEQ.

#+(or)
(defmethod convert-special
    ((symbol (eql 'cleavir-primop:typeq)) cst env system)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (typeq-cst arg1-cst arg2-cst) cst
    (declare (ignore typeq-cst))
    (ast:make-typeq-ast
     (convert arg1-cst env system)
     (env:parse-type-specifier (cst:raw arg2-cst) env system)
     :origin origin)))

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

#+(or)
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
                        :origin origin)
                       (loop for body in dests
                             collect (process-progn
                                      (convert-sequence body env system)
                                      origin))
                       (process-progn
                        (convert-sequence (cst:rest default) env system)
                        origin)
                       :origin origin))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:VALUES.
;;;
;;; This primitive operation can be used to inline CL:VALUES.  That
;;; is, (cl:values ...) and (cleavir-primop:values ...) are
;;; equivalent. The difference is that CL:VALUES is a function.  In
;;; the resulting HIR code, the use of this operation will appear as a
;;; FIXED-TO-MULTIPLE-INSTRUCTION.

#+(or)
(defmethod convert-special
    ((symbol (eql 'cleavir-primop:values)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (cst:db origin (values-cst . arguments-cst) cst
    (declare (ignore values-cst))
    (make-instance 'ast:values-ast
     :argument-asts (mapcar
		     (lambda (cst) (convert cst env system))
		     (cst:listify arguments-cst))
     :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:MULTIPLE-VALUE-SETQ
;;;
;;; This primitive operation can be used to compile
;;; CL:MULTIPLE-VALUE-SETQ. Unlike that operator, it requires all
;;; the variables to be lexical.

;;; Internal helper
(defun find-lexical-variable (var env sys)
  (assert (symbolp var))
  (let ((info (env:variable-info sys env var)))
    (assert (typep info 'env:lexical-variable-info))
    (env:identity info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:CAR, CDR, RPLACA, RPLACD.

(defprimop cleavir-primop:car)
(defprimop cleavir-primop:cdr)
(defprimop cleavir-primop:rplaca)
(defprimop cleavir-primop:rplacd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-ADD.

#+(or)
(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-add)) cst env system)
  (check-simple-primop-syntax cst 3)
  (cst:db origin (add-cst arg1-cst arg2-cst variable-cst) cst
    (declare (ignore add-cst))
    (ast:make-fixnum-add-ast (convert arg1-cst env system)
                                     (convert arg2-cst env system)
                                     (find-lexical-variable
                                      (cst:raw variable-cst) env system)
                                     :origin origin)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-SUB.

#+(or)
(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-sub)) cst env system)
  (check-simple-primop-syntax cst 3)
  (cst:db origin (sub-cst arg1-cst arg2-cst variable-cst) cst
    (declare (ignore sub-cst))
    (ast:make-fixnum-sub-ast (convert arg1-cst env system)
                                     (convert arg2-cst env system)
                                     (find-lexical-variable
                                      (cst:raw variable-cst) env system)
                                     :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-LESS.

(defprimop cleavir-primop:fixnum-less)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-NOT-GREATER.

#+(or)
(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-not-greater)) cst env system)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (not-greater-cst arg1-cst arg2-cst) cst
    (declare (ignore not-greater-cst))
    (make-instance 'ast:fixnum-not-greater-ast
      :arg1-ast (convert arg1-cst env system)
      :arg2-ast (convert arg2-cst env system)
      :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-GREATER.

#+(or)
(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-greater)) cst env system)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (greater-cst arg1-cst arg2-cst) cst
    (declare (ignore greater-cst))
    (make-instance 'ast:fixnum-greater-ast
      :arg1-ast (convert arg1-cst env system)
      :arg2-ast (convert arg2-cst env system)
      :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-NOT-LESS.

#+(or)
(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-not-less)) cst env system)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (not-less-cst arg1-cst arg2-cst) cst
    (declare (ignore not-less-cst))
    (make-instance 'ast:fixnum-not-less-ast
      :arg1-ast (convert arg1-cst env system)
      :arg2-ast (convert arg2-cst env system)
      :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-EQUAL.

#+(or)
(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-equal)) cst env system)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (equal-cst arg1-cst arg2-cst) cst
    (declare (ignore equal-cst))
    (make-instance 'ast:fixnum-equal-ast
      :arg1-ast (convert arg1-cst env system)
      :arg2-ast (convert arg2-cst env system)
      :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:LET-UNINITIALIZED.
;;;
;;; A form using the operator LET-UNINITIALIZED has the following
;;; syntax:
;;;
;;; (CLEAVIR-PRIMOP:LET-UNINITIALIZED (var*) form*)
;;;
;;; It sole purpose is to create a lexical environment for the
;;; variables in which the forms are evaluated.  An absolute
;;; requirement is that the variables must be assigned to before they
;;; are used in the forms, or else things will fail spectacularly.

#+(or)
(defmethod convert-special
    ((symbol (eql 'cleavir-primop:let-uninitialized)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (let-cst variables-cst . body-cst) cst
    (declare (ignore let-cst))
    (assert (cst:proper-list-p variables-cst))
    (assert (every #'symbolp (cst:raw variables-cst)))
    (let ((new-env env))
      (loop for rest-cst = variables-cst then (cst:rest rest-cst)
            until (cst:null rest-cst)
            do (let* ((variable-cst (cst:first rest-cst))
                      (variable (cst:raw variable-cst))
                      (variable-ast (ast:make-lexical-ast
                                     variable
                                     :origin (cst:source variable-cst))))
                 (setf new-env
                       (env:add-lexical-variable
                        new-env variable variable-ast))))
      (process-progn (convert-sequence body-cst new-env system)))))

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

#+(or)
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
     :origin origin
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

#+(or)
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
     :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:SLOT-READ, SLOT-WRITE,
;;; FUNCALLABLE-SLOT-READ, FUNCALLABLE-SLOT-WRITE.

(defprimop cleavir-primop:slot-read)
(defprimop cleavir-primop:slot-write)
(defprimop cleavir-primop:funcallable-slot-read)
(defprimop cleavir-primop:funcallable-slot-write)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:AREF.
;;;
;;; This primop takes five arguments. The first and second are an
;;; array and an index into it, as forms. The remainder of the
;;; arguments are not evaluated. The third is the actual element
;;; type of the array. The fourth is whether the array is actually
;;; simple. The fifth is whether the value in the array is already
;;; boxed.

#+(or)
(defmethod convert-special
    ((symbol (eql 'cleavir-primop:aref)) cst env system)
  (check-simple-primop-syntax cst 5)
  (cst:db origin (aref-cst array-cst index-cst type-cst simple-p-cst boxed-p-cst) cst
    (declare (ignore aref-cst))
    (make-instance 'ast:aref-ast
      :array-ast (convert array-cst env system)
      :index-ast (convert index-cst env system)
      :element-type (cst:raw type-cst)
      :simple-p (cst:raw simple-p-cst)
      :boxed-p (cst:raw boxed-p-cst)
      :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:ASET.
;;;
;;; This primop takes six arguments. The first two are an array and
;;; an index into it. The third is the object to be written in. The
;;; fourth and fifth are as above. The sixth is whether the objects
;;; in the array are boxed.
;;;
;;; Forms using this primitive operation must occur in a context
;;; that does not require a value, such as in a PROGN other than as
;;; the last form.

#+(or)
(defmethod convert-special
    ((symbol (eql 'cleavir-primop:aset)) cst env system)
  (check-simple-primop-syntax cst 6)
  (cst:db origin (aset-cst array-cst index-cst object-cst type-cst simple-p-cst boxed-p-cst)
      cst
    (declare (ignore aset-cst))
    (make-instance 'ast:aset-ast
      :array-ast (convert array-cst env system)
      :index-ast (convert index-cst env system)
      :element-ast (convert object-cst env system)
      :element-type (cst:raw type-cst)
      :simple-p (cst:raw simple-p-cst)
      :boxed-p (cst:raw boxed-p-cst)
      :origin origin)))

;;; The following macro is used to generate a method on
;;; CONVERT-SPECIAL for binary floating-point primops.
(defmacro define-float-binop (primop ast)
  (declare (ignore primop ast))
  #+(or)
  `(defmethod convert-special
       ((symbol (eql ',primop)) cst env system)
     (check-simple-primop-syntax cst 3)
     (cst:db origin (op-cst type-cst arg1-cst arg2-cst) cst
       (declare (ignore op-cst))
       (make-instance ',ast
         :subtype (cst:raw type-cst)
         :arg1-ast (convert arg1-cst env system)
         :arg2-ast (convert arg2-cst env system)
         :origin origin))))

#+(or)
(progn
(define-float-binop cleavir-primop:float-add
  ast:float-add-ast)
(define-float-binop cleavir-primop:float-sub
  ast:float-sub-ast)
(define-float-binop cleavir-primop:float-mul
  ast:float-mul-ast)
(define-float-binop cleavir-primop:float-div
  ast:float-div-ast)
(define-float-binop cleavir-primop:float-less
  ast:float-less-ast)
(define-float-binop cleavir-primop:float-not-greater
  ast:float-not-greater-ast)
(define-float-binop cleavir-primop:float-equal
  ast:float-equal-ast)
(define-float-binop cleavir-primop:float-not-less
  ast:float-not-less-ast)
(define-float-binop cleavir-primop:float-greater
  ast:float-greater-ast)
)

;;; The following macro is used to generate a method on
;;; CONVERT-SPECIAL for unatry floating-point primops.
(defmacro define-float-unop (primop ast)
  (declare (ignore primop ast))
  #+(or)
  `(defmethod convert-special
       ((symbol (eql ',primop)) cst env system)
     (check-simple-primop-syntax cst 2)
     (cst:db origin (op-cst type-cst arg-cst) cst
       (declare (ignore op-cst))
       (make-instance ',ast
         :subtype (cst:raw type-cst)
         :arg-ast (convert arg-cst env system)
         :origin origin))))

#+(or)
(progn
(define-float-unop cleavir-primop:float-sin
  ast:float-sin-ast)

(define-float-unop cleavir-primop:float-cos
  ast:float-cos-ast)

(define-float-unop cleavir-primop:float-sqrt
  ast:float-sqrt-ast)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:COERCE.
;;;
;;; This primop is used to convert between number types. It takes
;;; three arguments. The first two are number types, and the last
;;; is a form, the only argument that is evaluated.
;;;
;;; The value of the form must be of the number type. It is
;;; converted into a new value of the second type.
;;;
;;; This primop can be used for implementing CL:COERCE and CL:FLOAT,
;;; as well as for arithmetic contagion.

#+(or)
(defmethod convert-special
    ((symbol (eql 'cleavir-primop:coerce)) cst env system)
  (check-simple-primop-syntax cst 3)
  (cst:db origin (op-cst type1-cst type2-cst form-cst) cst
    (declare (ignore op-cst))
    (make-instance 'ast:coerce-ast
     :from (cst:raw type1-cst) :to (cst:raw type2-cst)
     :arg-ast (convert form-cst env system)
     :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:UNREACHABLE.
;;;
;;; Recall that this primop indicates that execution of the form
;;; should be impossible.

#+(or)
(defmethod convert-special
    ((symbol (eql 'cleavir-primop:unreachable)) cst env system)
  (declare (ignore env system))
  (check-simple-primop-syntax cst 0)
  (make-instance 'ast:unreachable-ast :origin (cst:source cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:CST-TO-AST
;;;
;;; This primop can be used to access ASTs from generated code,
;;; which is useful for e.g. saving inline definitions.
;;; Its argument, a form, is converted to an AST and then treated as
;;; a literal- similar to `',(cst-to-ast form ...), but with the form
;;; being converted from a CST like anything else.
;;; The environment used is the current environment, stripped of
;;; runtime bindings.

#+(or)
(defmethod convert-special
    ((symbol (eql 'cleavir-primop:cst-to-ast)) cst env system)
  (check-simple-primop-syntax cst 2)
  (cst:db origin (op-cst form-cst use-file-compilation-semantics-p) cst
    (declare (ignore op-cst))
    (let ((*compiler* (if (cst:raw use-file-compilation-semantics-p)
                          'cl:compile-file
                          'cl:compile)))
      (ast:make-constant-ast
       (convert form-cst (env:compile-time env) system)
       :origin (cst:source form-cst)))))
