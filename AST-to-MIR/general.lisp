(cl:in-package #:cleavir-ast-to-mir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compilation context.
;;;
;;; Each AST is compiled in a particular COMPILATION CONTEXT or
;;; CONTEXT for short.  A context object has two components: 
;;;
;;; 1. SUCCESSORS, which is a proper list containing zero, one or two
;;; elements.  These elements are instructions resulting from the
;;; generation of the code that should be executed AFTER the code
;;; generated from this AST.  If the list contains two elements, then
;;; this AST is compiled in a context where a Boolean result is
;;; required.  In this case, the first element of the list is the
;;; successor to use when the value generated by the AST is NIL, and
;;; the second element is the successor to use when the value
;;; generated by the AST is something other than NIL.  If there are no
;;; successors, as indicated by SUCCESSORS being the empty list, then
;;; either a TAILCALL-INSTRUCTION (if the AST is a CALL-AST) or a
;;; RETURN-INSTRUCTION (for other ASTs) that returns all the values of
;;; the AST should be generated.
;;;
;;; 2. RESULTS, a proper list indicating how many values are required
;;; from the compilation of this AST.  It contains a list of lexical
;;; locations into which the generated code must put the values of
;;; this AST.  If the list is empty, it means either that no values
;;; are required (when (PLUSP (LENGTH SUCCESSORS))) or that ALL values
;;; are requried (when (ZEROP (LENGTH SUCCESSORS))).  If the list
;;; contains more elements than the number of values generated by this
;;; AST, then the remaining lexical locations in the list must be
;;; filled with NIL by the code generated from this AST.
;;;
;;; The following combinations can occur:
;;;
;;;   SUCCESSORS is the empty list.  Then RESULTS is also the empty
;;;   list, which means that ALL the values are required.  Forms that
;;;   are in TAIL POSITION are compiled in a context like this.
;;;
;;;   SUCCESSORS has one element.  then RESULTS can have any number of
;;;   elements.
;;;
;;;      If RESULTS has no elements, this means that no values are
;;;      required.  Forms inside a PROGN other than the last are
;;;      compiled in a context like this.
;;;
;;;      If RESULTS has a single element, then a single value is
;;;      required.  Arguments to function calls are examples of ASTs
;;;      that are compiled in a context like this.
;;;
;;;      If RESULTS has more than one element, then that many values
;;;      are required.  The VALUES-FORM-AST of MULTIPLE-VALUE-BIND-AST
;;;      is compiled in a context like this.
;;;
;;;   SUCCESSOR has two elements.  Then RESULTS is the empty list,
;;;   meaning that no values are required.  The TEST-AST of an IF-AST
;;;   is compiled in a context like this. 
;;;
;;;   SUCCESSORS has more than two elements.  This possibility is
;;;   currently not used.  It is mean to be used for forms like CASE,
;;;   TYPECASE, etc.  Again, the RESULTS would be the empty list. 

(defclass context ()
  ((%results :initarg :results :reader results)
   (%successors :initarg :successors :accessor successors)))

(defun context (results successors)
  (unless (and (listp results)
	       (every (lambda (result)
			(typep result 'cleavir-mir:lexical-location))
		      results))
    (error "illegal results: ~s" results))
  (unless (and (listp successors)
	       (every (lambda (successor)
			(typep successor 'cleavir-mir:instruction))
		      successors))
    (error "illegal successors: ~s" results))
  (when (and (null successors) (not (null results)))
    (error "Illegal combination of results and successors"))
  (make-instance 'context
    :results results
    :successors successors))

(defmethod print-object ((obj context) stream)
  (print-unreadable-object (obj stream)
    (format stream " results: ~s" (results obj))
    (format stream " successors: ~s" (successors obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; During compilation, this variable contains a hash table that maps
;;; ASTs representing locations to MIR locations.

(defparameter *location-info* nil)

(defun find-or-create-location (ast)
  (or (gethash ast *location-info*)
      (let ((location
	      (etypecase ast
		(cleavir-ast:lexical-ast
		 (cleavir-mir:make-lexical-location
		  (cleavir-ast:name ast)))
		(cleavir-ast:global-ast
		 (cleavir-mir:make-global-input
		  (cleavir-ast:name ast)))
		(cleavir-ast:special-ast
		 (cleavir-mir:make-special-location
		  (cleavir-ast:name ast))))))
	(setf (gethash ast *location-info*) location))))

(defun make-temp (argument)
  (declare (ignore argument))
  (cleavir-mir:new-temporary))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an abstract syntax tree in a compilation context.
;;;
;;; The result of the compilation is a single value, namely the first
;;; instruction of the instruction graph resulting from the
;;; compilation of the entire AST.

;;; Given a list of results and a successor, generate a sequence of
;;; instructions preceding that successor, and that assign NIL to each
;;; result in the list.
(defun nil-fill (results successor)
  (let ((next successor))
    (loop for value in results
	  do (setf next
		   (cleavir-mir:make-assignment-instruction
		    (cleavir-mir:make-constant-input 'nil)
		    value next))
	  finally (return next))))

(defgeneric compile-ast (ast context))

;;; When an AST that is meant for a test (as indicated by it being an
;;; instance of BOOLEAN-AST-MIXIN) is compiled in a context where one
;;; or more values are needed, we generate two branches; one where NIL
;;; is assigned to the first result and one where T is assigned to it
;;; (remaining results are filled with NIL).  Then we compile the AST
;;; in a context with the two branches and no result.
(defmethod compile-ast :around ((ast cleavir-ast:boolean-ast-mixin) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (ecase (length successors)
      (1
       (let* ((result (car results))
	      (successor (nil-fill (cdr results) (car successors)))
	      (true (cleavir-mir:make-constant-input T))
	      (false (cleavir-mir:make-constant-input NIL))
	      (true-branch (make-instance 'cleavir-mir:assignment-instruction
			     :inputs (list true)
			     :outputs (list result)
			     :successors (list successor)))
	      (false-branch (make-instance 'cleavir-mir:assignment-instruction
			      :inputs (list false)
			      :outputs (list result)
			      :successors (list successor))))
	 (call-next-method ast (context '() (list true-branch false-branch)))))
      (2
       (call-next-method)))))

(defun check-context-for-boolean-ast (context)
  (assert (and (zerop (length (results context)))
	       (= (length (successors context)) 2))))

(defmethod compile-ast :around ((ast cleavir-ast:one-value-ast-mixin) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (ecase (length successors)
      (0
       ;; There are no successors, which means that this AST is
       ;; compiled in a context where the value(s) should be returned
       ;; to the caller.  We deal with this situation by creating a
       ;; context that has a RETURN-INSTRUCTION in it, and we compile
       ;; the AST in that context instead.
       (let* ((temp (cleavir-mir:new-temporary))
	      (successor (cleavir-mir:make-return-instruction (list temp))))
	 (call-next-method ast (context (list temp) (list successor)))))
      (1
       ;; We have a context with one successor, so the list of results
       ;; can have any length.
       (if (null results)
	   ;; We don't need the result.  This situation typically
	   ;; happens when we compile a form other than the last of a
	   ;; PROGN-AST.
	   (if (cleavir-ast:side-effect-free-p ast)
	       (progn
		 ;; Warn an generate no code.
		 (warn "Form compiled in a context requiring no value.")
		 (car successors))
	       ;; We allocate a temporary variable to receive the
	       ;; result, and that variable will not be used.
	       (call-next-method ast (context (list (make-temp nil)) successors)))
	   ;; We have at least one result.  In case there is more than
	   ;; one, we generate a successor where all but the first one
	   ;; are filled with NIL. 
	   (let ((successor (nil-fill (cdr results) (car successors))))
	     (call-next-method ast (context (list (car results))
					    (list successor))))))
      (2
       ;; We have a context where a test of a Boolean is required.  We
       ;; create a new context where the result is compared to NIL
       ;; using EQ-INSTRUCTION, and compile the AST in that context
       ;; instead. 
       (let* ((false (cleavir-mir:make-constant-input NIL))
	      (temp (cleavir-mir:new-temporary))
	      (successor (make-instance 'cleavir-mir:eq-instruction
			   :inputs (list temp false)
			   :outputs '()
			   :successors (reverse successors))))
	 (call-next-method ast (context (list temp) (list successor))))))))

(defun check-context-for-one-value-ast (context)
  (assert (and (= (length (results context)) 1)
	       (= (length (successors context)) 1))))

(defun check-context-for-no-value-ast (context)
  (assert (and (zerop (length (results context)))
	       (= (length (successors context)) 1))))

;;; This function is used by compile methods that need a single
;;; successor and that produce a single result.  It takes an arbitrary
;;; context as an argument and returns two values, the successor and a
;;; location for the result. 
(defun adapt-context-1-1 (context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (ecase (length successors)
      (0
       (let ((location (make-temp nil)))
	 (values (cleavir-mir:make-return-instruction (list location))
		 location)))
      (1
       (if (null results)
	   (values (car successors)
		   (make-temp nil))
	   (values (nil-fill (cdr results) (car successors))
		   (car results))))
      (2
       (let ((location (if (null results) (make-temp nil) (car results))))
	 (values (cleavir-mir:make-eq-instruction
		  (list location (cleavir-mir:make-constant-input 'nil))
		  successors)
		 location))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile ASTs that represent Common Lisp operations. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an IF-AST.  
;;;
;;; We compile the test of the IF-AST in a context where no value is
;;; required and with two successors, the else branch and the then
;;; branch.  The two branches are compiled in the same context as the
;;; IF-AST itself.

(defmethod compile-ast ((ast cleavir-ast:if-ast) context)
  (let ((then-branch (compile-ast (cleavir-ast:then-ast ast) context))
	(else-branch (compile-ast (cleavir-ast:else-ast ast) context)))
    (compile-ast (cleavir-ast:test-ast ast)
		 (context '() (list then-branch else-branch)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a PROGN-AST.
;;;
;;; The last sub-ast is compiled in the same context as the progn-ast
;;; itself.  All the others are copiled in a context where no value is
;;; required, and with the code for the following form as a single
;;; successor.

(defmethod compile-ast ((ast cleavir-ast:progn-ast) context)
  (let ((next (compile-ast (car (last (cleavir-ast:form-asts ast))) context)))
    (loop for sub-ast in (cdr (reverse (cleavir-ast:form-asts ast)))
	  do (setf next (compile-ast sub-ast (context '() (list next)))))
    next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a BLOCK-AST.
;;;
;;; A BLOCK-AST is compiled by compiling its body in the same context
;;; as the block-ast itself.  However, we store that context in the
;;; *BLOCK-INFO* hash table using the block-ast as a key, so that a
;;; RETURN-FROM-AST that refers to this block can be compiled in the
;;; same context.

(defparameter *block-info* nil)

(defmethod compile-ast ((ast cleavir-ast:block-ast) context)
  (setf (gethash ast *block-info*) context)
  (compile-ast (cleavir-ast:body-ast ast) context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a RETURN-FROM-AST.
;;;
;;; A RETURN-FROM-AST is compiled as follows: The context is ignored,
;;; because the RETURN-FROM does not return a value in its own
;;; context.  Instead, the FORM-AST of the RETURN-FROM-AST is compiled
;;; in the same context as the corresponding BLOCK-AST was compiled
;;; in.

(defmethod compile-ast ((ast cleavir-ast:return-from-ast) context)
  (declare (ignore context))
  (let ((block-context (gethash (cleavir-ast:block-ast ast) *block-info*)))
    (compile-ast (cleavir-ast:form-ast ast) block-context)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a TAGBODY-AST.
;;;
;;; A TAGBODY-AST is compiled as follows: 
;;;
;;; A single successor is determined as follows:
;;;
;;;   * If the context has no successors, then a RETURN-INSTRUCTION
;;;     which returns a single NIL value is generated, and that
;;;     RETURN-INSTRUCTION becomes the successor.
;;; 
;;;   * If the context has one or more successors and RESULTS is an
;;;     empty list, the successor will be the first in the list.  The
;;;     reason that this method always works, is that if there are
;;;     several successors, then the first one should be chosen when
;;;     the AST yields NIL, which is always the case for the
;;;     TAGBODY-AST.
;;; 
;;;   * If the context has one or more successors and RESULTS contains
;;;     at least one element, then the successor is the first
;;;     instruction in a sequence of instructions that fill the
;;;     results with NIL.
;;; 
;;; For each TAG-AST in the tagbody, a NOP instruction is created and
;;; that instruction is entered into the hash table *GO-INFO* using
;;; the TAG-AST as a key.  Then the items are compiled in the reverse
;;; order, stacking new instructions before the successor computed
;;; previously.  Compiling a TAG-AST results in the successor of the
;;; corresponding NOP instruction being modified to point to the
;;; remining instructions already computed.  Compiling something else
;;; is done in a context with an empty list of results, using the
;;; remaining instructions already computed as a single successor.

(defparameter *go-info* nil)

(defmethod compile-ast ((ast cleavir-ast:tagbody-ast) context)
  (loop for item in (cleavir-ast:items ast)
	do (when (typep item 'cleavir-ast:tag-ast)
	     (setf (gethash item *go-info*)
		   (cleavir-mir:make-nop-instruction nil))))
  (with-accessors ((results results)
		   (successors successors))
      context
    (let ((next (cond ((null successors)
		       (cleavir-mir:make-return-instruction
			(list (cleavir-mir:make-constant-input 'nil))))
		      ((null results)
		       (car successors))
		      (t
		       (nil-fill results (car successors))))))
      (loop for item in (reverse (cleavir-ast:items ast))
	    do (setf next
		     (if (typep item 'cleavir-ast:tag-ast)
			 (let ((instruction (gethash item *go-info*)))
			   (setf (cleavir-mir:successors instruction)
				 (list next))
			   instruction)
			 (compile-ast item (context '() (list next))))))
      next)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a GO-AST.
;;;
;;; The CONTEXT is ignored.  Instead, the successor becomes the NOP
;;; instruction that was entered into the hash table *GO-INFO* when
;;; the TAGBODY-AST was compiled.

(defmethod compile-ast ((ast cleavir-ast:go-ast) context)
  (declare (ignore context))
  (gethash (cleavir-ast:tag-ast ast) *go-info*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CALL-AST.
;;;
;;; If the CALL-AST is compiled in a context with no successors, then
;;; generate a TAILCALL-INSTRUCTION.
;;;
;;; If there is at least one successor, then we must put the values
;;; generated by the call into the syntactic location indicated by the
;;; RESULTS.  This is done by the GET-VALUES-INSTRUCTION.  That
;;; instruction may use one or two successors.  If it has two
;;; successors, tests the first value received and selects a successor
;;; based on whether that value is NIL or something else.

(defmethod compile-ast ((ast cleavir-ast:call-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (let* ((all-args (cons (cleavir-ast:callee-ast ast)
			   (cleavir-ast:argument-asts ast)))
	   (temps (make-temps all-args)))
      (compile-arguments
       all-args
       temps
       ;; This is a temporary kludge to take advantage of the fact
       ;; that the function ERROR never returns.  A more systematic
       ;; approach would be to check that the function is system
       ;; supplied and that its return type is NIL.
       (if (and (typep (cleavir-ast:callee-ast ast) 'cleavir-ast:global-ast)
		(eq (cleavir-ast:name (cleavir-ast:callee-ast ast)) 'error))
	   (cleavir-mir:make-funcall-instruction temps)
	   (ecase (length successors)
	     (0 (cleavir-mir:make-tailcall-instruction temps))
	     (1 (make-instance 'cleavir-mir:funcall-instruction
		  :inputs temps
		  :outputs results
		  :successors successors))
	     (2 (let* ((temp (cleavir-mir:new-temporary))
		       (false (cleavir-mir:make-constant-input nil))
		       (successor (make-instance 'cleavir-mir:eq-instruction
				    :inputs (list temp false)
				    :outputs '()
				    :successors (reverse successors))))
		  (make-instance 'cleavir-mir:funcall-instruction
		    :inputs temps
		    :outputs (list temp)
		    :successors (list successor))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FUNCTION-AST.
;;;
;;; The FUNCTION-AST represents a closure, so we compile it by
;;; compiling its LAMBDA-LIST and BODY-AST into some code, represented
;;; by the first instruction in the body.  We then generate an
;;; ENCLOSE-INSTRUCTION that takes this code as input.
;;;
;;; The value computed by the FUNCTION-AST is always a function, so it
;;; is always a single non-NIL value.  If there are no successors, we
;;; also generate a RETURN-INSTRUCTION with the single value as input.
;;; If there is more than one successor, chose the second one for the
;;; true value.

(defun translate-lambda-list (lambda-list)
  (loop for item in lambda-list
	collect (cond ((member item lambda-list-keywords)
		       item)
		      ((consp item)
		       (if (= (length item) 3)
			   (list (first item)
				 (find-or-create-location (second item))
				 (find-or-create-location (third item)))
			   (list (find-or-create-location (second item))
				 (find-or-create-location (third item)))))
		      (t
		       (find-or-create-location item)))))

(defmethod compile-ast ((ast cleavir-ast:function-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (let* ((body (compile-ast (cleavir-ast:body-ast ast) (context '() '())))
	   (ll (translate-lambda-list (cleavir-ast:lambda-list ast)))
	   (function (cleavir-mir:make-enter-instruction ll body)))
      (ecase (length successors)
	(0 (let ((temp (cleavir-mir:new-temporary)))
	     (cleavir-mir:make-enclose-instruction
	      temp
	      (cleavir-mir:make-return-instruction (list temp))
	      function)))
	(1 (if (null results)
	       (progn (warn "closure compiled in a context with no values")
		      (car successors))
	       (cleavir-mir:make-enclose-instruction
		(car results)
		(nil-fill (cdr results) (car successors))
		function)))
	(2 (if (null results)
	       (car successors)
	       (cleavir-mir:make-enclose-instruction
		(car results)
		(nil-fill (cdr results) (cadr successors))
		function)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SETQ-AST.

(defmethod compile-ast ((ast cleavir-ast:setq-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (let* ((location (find-or-create-location (cleavir-ast:lhs-ast ast)))
	   (next 
	     (ecase (length successors)
	       (0 (cleavir-mir:make-return-instruction (list location)))
	       (1 (if (null results)
		      (car successors)
		      (cleavir-mir:make-assignment-instruction
		       location
		       (car results)
		       (nil-fill (cdr results) (car successors)))))
	       (2 (if (null results)
		      (cleavir-mir:make-eq-instruction
		       (list location (cleavir-mir:make-constant-input 'nil))
		       successors)
		      (cleavir-mir:make-assignment-instruction
		       location
		       (car results)
		       (cleavir-mir:make-eq-instruction
			(list location (cleavir-mir:make-constant-input 'nil))
			successors)))))))
      (compile-ast (cleavir-ast:value-ast ast)
		   (context (list location) (list next))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a THE-AST.

(defun make-type-check (type-ast var successor)
  (check-type type-ast cleavir-ast:constant-ast)
  (let* ((type-input (cleavir-mir:make-constant-input (cleavir-ast:value type-ast)))
	 (error-branch
	   (cleavir-mir:make-funcall-instruction
	    (list (cleavir-mir:make-global-input 'error)
		  (cleavir-mir:make-constant-input :datum)
		  var
		  (cleavir-mir:make-constant-input :expected-type)
		  type-input))))
    (cleavir-mir:make-typeq-instruction
     (list var type-input)
     (list error-branch successor))))

(defmethod compile-ast ((ast cleavir-ast:the-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (destructuring-bind (form-ast . type-asts)
	(cleavir-ast:children ast)
      (ecase (length successors)
	(0
	 ;; This case is a bit hard to handle, because we don't know a
	 ;; priori how many values are returned by the FORM-AST if the
	 ;; THE-AST.  For now, do what we are allowed to do according to
	 ;; the HyperSpec (i.e., don't check the types).  Eventually we
	 ;; hope to implement a better solution.
	 (compile-ast form-ast context))
	(1
	 (let* ((temp-count (max (length results) (length type-asts)))
		(temps (make-temps (make-list temp-count))))
	   (let ((next (car successors)))
	     ;; The last actions to take are to assign the temps to
	     ;; the results.
	     (loop for result in results
		   for temp in temps
		   do (setf next
			    (cleavir-mir:make-assignment-instruction
			     temp result next)))
	     ;; Before assigning to the results, check the
	     ;; types of the values.
	     (loop for type-ast in type-asts
		   for temp in temps
		   do (setf next (make-type-check type-ast temp next)))
	     next)))
	(2
	 (if (null results)
	     (let ((temp (make-temp nil)))
	       (compile-ast
		ast
		(context
		 (list temp)
		 (list (cleavir-mir:make-eq-instruction
			(list temp (cleavir-mir:make-constant-input 'nil))
			successors)))))
	     (compile-ast
	      ast
	      (context
	       (list (car results))
	       (list (nil-fill
		      (cdr results)
		      (cleavir-mir:make-eq-instruction
		       (list (car results)
			     (cleavir-mir:make-constant-input 'nil))
		       successors)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a TYPEQ-AST.

(defun make-boolean (boolean result successor)
  (cleavir-mir:make-assignment-instruction
   (cleavir-mir:make-external-input boolean)
   result
   successor))

(defmethod compile-ast ((ast cleavir-ast:typeq-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (ecase (length successors)
      (0 
       (let* ((temp1 (make-temp nil))
	      (next (cleavir-mir:make-return-instruction (list temp1)))
	      (false (make-boolean nil temp1 next))
	      (true (make-boolean t temp1 next))
	      (temp2 (make-temp nil)))
	 (compile-ast
	  (cleavir-ast:form-ast ast)
	  (context
	   (list temp2)
	   (list (cleavir-mir:make-typeq-instruction
		  (list temp2
			(cleavir-mir:make-constant-input
			 (cleavir-ast:type-specifier ast)))
		  (list false true)))))))
      (1 (if (null results)
	     (progn (warn "test compiled in a context with no results")
		    (car successors))
	     (let* ((false (make-boolean nil (car results) (car successors)))
		    (true (make-boolean t (car results) (car successors)))
		    (temp (make-temp nil)))
	       (compile-ast
		(cleavir-ast:form-ast ast)
		(context
		 (list temp)
		 (list
		  (nil-fill
		   (cdr results)
		   (cleavir-mir:make-typeq-instruction
		    (list temp
			  (cleavir-mir:make-constant-input
			   (cleavir-ast:type-specifier ast)))
		    (list false true)))))))))
      (2 (if (null results)
	     (let ((temp (make-temp nil)))
	       (compile-ast
		(cleavir-ast:form-ast ast)
		(context
		 (list temp)
		 (list (cleavir-mir:make-typeq-instruction
			(list temp
			      (cleavir-mir:make-constant-input
			       (cleavir-ast:type-specifier ast)))
			successors)))))
	     (let ((false (make-boolean nil (car results) (car successors)))
		   (true (make-boolean t (car results) (cadr successors)))
		   (temp (make-temp nil)))
	       (compile-ast
		(cleavir-ast:form-ast ast)
		(context
		 (list temp)
		 (list
		  (nil-fill
		   (cdr results)
		   (cleavir-mir:make-typeq-instruction
		    (list temp
			  (cleavir-mir:make-constant-input
			   (cleavir-ast:type-specifier ast)))
		    (list false true))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a LEXICAL-AST.
;;;
;;; This AST has ONE-VALUE-AST-MIXIN as a superclass. 

(defmethod compile-ast ((ast cleavir-ast:lexical-ast) context)
  (check-context-for-one-value-ast context)
  (cleavir-mir:make-assignment-instruction
   (find-or-create-location ast)
   (first (results context))
   (first (successors context))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a GLOBAL-AST.
;;;
;;; This AST has ONE-VALUE-AST-MIXIN as a superclass. 

(defmethod compile-ast ((ast cleavir-ast:global-ast) context)
  (check-context-for-one-value-ast context)
  (cleavir-mir:make-assignment-instruction
   (find-or-create-location ast)
   (first (results context))
   (first (successors context))))

(defun compile-toplevel (ast)
  (let ((*block-info* (make-hash-table :test #'eq))
	(*go-info* (make-hash-table :test #'eq))
	(*location-info* (make-hash-table :test #'eq)))
    ;; The top-level ast must represent a thunk.
    (assert (typep ast 'cleavir-ast:function-ast))
    (let* ((body (compile-ast (cleavir-ast:body-ast ast) (context '() '())))
	   (ll (translate-lambda-list (cleavir-ast:lambda-list ast))))
      (cleavir-mir:make-enter-instruction ll body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile ASTs that represent low-level operations.

(defun make-temps (arguments)
  (loop for argument in arguments
	collect (make-temp argument)))

(defun compile-arguments (arguments temps successor)
  (loop with succ = successor
	for arg in (reverse arguments)
	for temp in (reverse temps)
	do (unless (or (typep temp 'cleavir-mir:immediate-input)
		       (typep temp 'cleavir-mir:external-input))
	     (setf succ (compile-ast arg (context `(,temp) `(,succ)))))
	finally (return succ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CONSTANT-AST.
;;;

(defmethod compile-ast ((ast cleavir-ast:constant-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (ecase (length successors)
      (0 (cleavir-mir:make-return-instruction
	  (list (cleavir-mir:make-constant-input (cleavir-ast:value ast)))))
      (1 (if (null results)
	     (progn 
	       (warn "constant compiled in a context with no values")
	       (car successors))
	     (cleavir-mir:make-assignment-instruction
	      (cleavir-mir:make-constant-input (cleavir-ast:value ast))
	      (car results)
	      (nil-fill (cdr results) (car successors)))))
      (2 (if (null results)
	     (if (null (cleavir-ast:value ast))
		 (car successors)
		 (cadr successors))
	     (cleavir-mir:make-assignment-instruction
	      (cleavir-mir:make-constant-input (cleavir-ast:value ast))
	      (car results)
	      (if (null (cleavir-ast:value ast))
		  (car successors)
		  (cadr successors))))))))
