(cl:in-package #:cleavir-ast)

;;;; We define the abstract syntax trees (ASTs) that represent not
;;;; only Common Lisp code, but also the low-level operations that we
;;;; use to implement the Common Lisp operators that can not be
;;;; portably implemented using other Common Lisp operators.
;;;;
;;;; The AST is a very close representation of the source code, except
;;;; that the environment is no longer present, so that there are no
;;;; longer any different namespaces for functions and variables.  And
;;;; of course, operations such as MACROLET are not present because
;;;; they only alter the environment.
;;;;
;;;; The AST form is the preferred representation for some operations;
;;;; in particular for PROCEDURE INTEGRATION (sometimes called
;;;; INLINING).

(defgeneric map-children (function ast)
  (:argument-precedence-order ast function)
  (:method-combination progn))

(defgeneric children (ast)
  (:method-combination append))

(defmacro define-children (ast-class children-spec)
  (multiple-value-bind (children rest-child)
      (if (symbolp children-spec)
          (values nil children-spec)
          (let* ((last (last children-spec))
                 (last-cdr (cdr last)))
            (if last-cdr
                (values (append (butlast children-spec) (list (car last))) last-cdr)
                (values children-spec nil))))
    `(progn
       (defmethod children append ((ast ,ast-class))
         ,(let ((access (loop for child in children
                              collect `(,child ast))))
            (if rest-child
                `(list* ,@access (,rest-child ast))
                `(list ,@access))))
       (defmethod map-children progn (function (ast ,ast-class))
         ,@(when (and (null children) (not rest-child))
             `((declare (cl:ignore function))))
         ,@(loop for child in children
                 collect `(funcall function (,child ast)))
         ,(when rest-child
            `(mapc function (,rest-child ast)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variable *POLICY*. Default for :policy initarg.
;;; This is useful because every AST has a policy, but they're
;;; shared very heavily and generated all over the place.

(defvar *policy*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class AST.  The base class for all AST classes.
;;;
;;; ORIGIN is a client-supplied object that is not interpreted by
;;; Cleavir.
;;; POLICY is the compilation policy in force for the AST.
;;; DYNAMIC-ENVIRONMENT is a lexical-ast representing the
;;; dynamic environment in force.

(defclass ast ()
  ((%origin :initform nil :initarg :origin :accessor origin)
   (%policy :initform *policy* :initarg :policy :accessor policy)))

;;; Policies must be saved
(cleavir-io:define-save-info ast
  (:origin origin)
  (:policy policy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin classes.

;;; This class is used as a superclass for ASTs that produce Boolean
;;; results, so are mainly used as the TEST-AST of an IF-AST.
(defclass boolean-ast-mixin () ())

;;; This class is used as a superclass for ASTs that produce results
;;; for BRANCH-AST.
(defclass multiway-ast-mixin () ())

;;; This class is used as a superclass for ASTs that produce no value
;;; and that must be compiled in a context where no value is required.
(defclass no-value-ast-mixin () ())

;;; This class is used as a superclass for ASTs that produce a single
;;; value that is not typically not just a Boolean value.
(defclass one-value-ast-mixin () ())

;;; This class is used as a superclass for ASTs that have no side
;;; effect.
(defclass side-effect-free-ast-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Predicate to test whether an AST is side-effect free.
;;;
;;; For instances of SIDE-EFFECT-FREE-AST-MIXIN, this predicate always
;;; returns true.  For others, it has a default method that returns
;;; false.  Implementations may add a method on some ASTs such as
;;; CALL-AST that return true only if a particular call is side-effect
;;; free.

(defgeneric side-effect-free-p (ast))

(defmethod side-effect-free-p (ast)
  (declare (cl:ignore ast))
  nil)

(defmethod side-effect-free-p ((ast side-effect-free-ast-mixin))
  (declare (ignorable ast))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AST classes for standard common lisp features.
;;;
;;; There is mostly a different type of AST for each Common Lisp
;;; special operator, but there are some exceptions.  Here are the
;;; Common Lisp special operators: BLOCK, CATCH, EVAL-WHEN, FLET,
;;; FUNCTION, GO, IF, LABELS, LET, LET*, LOAD-TIME-VALUE, LOCALLY,
;;; MACROLET, MULTIPLE-VALUE-CALL, MULTIPLE-VALUE-PROG1, PROGN, PROGV,
;;; QUOTE, RETURN-FROM, SETQ, SYMBOL-MACROLET, TAGBODY, THE, THROW,
;;; UNWIND-PROTECT.
;;;
;;; Some of these only influence the environment and do not need a
;;; representation as ASTs.  These are: LOCALLY, MACROLET, and
;;; SYMBOL-MACROLET.
;;;
;;; FLET and LABELS are like LET except that the symbols the bind are
;;; in the function namespace, but the distinciton between namespeces
;;; no longer exists in the AST.
;;;
;;; A LAMBDA expression, either inside (FUNCTION (LAMBDA ...)) or when
;;; it is the CAR of a compound form, compiles into a FUNCTION-AST.
;;; The FUNCTION special form does not otherwise require an AST
;;; because the other form of the FUNCTION special form is just a
;;; conversion between namespaces and again, namespaces are no longer
;;; present in the AST.
;;;
;;; We also define ASTs that do not correspond to any Common Lisp
;;; special operators, because we simplify later code generation that
;;; way.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IMMEDIATE-AST.
;;;
;;; This class represents constants that have a representation that
;;; allows them to be immediate values in the resulting machine code.
;;; Obviously, whether that is possible depends on the implementation.
;;;
;;; The value of the constant is represented as a possibly-negative
;;; integer that is the machine-code representation of the constant.

(defclass immediate-ast (one-value-ast-mixin side-effect-free-ast-mixin ast)
  ((%value :initarg :value :reader value)))

(defun make-immediate-ast (value &key origin (policy *policy*))
  (make-instance 'immediate-ast
    :origin origin :policy policy
    :value value))

(cleavir-io:define-save-info immediate-ast
  (:value value))

(define-children immediate-ast ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CONSTANT-AST.
;;;
;;; This class represents Lisp constants in source code.
;;;
;;; If the constant that was found was wrapped in QUOTE, then the
;;; QUOTE is not part of the value here, because it was stripped off.
;;;
;;; If the constant that was found was a constant variable, then the
;;; value here represents the value of that constant variable at
;;; compile time.

(defclass constant-ast (one-value-ast-mixin side-effect-free-ast-mixin ast)
  ((%value :initarg :value :reader value)))

(defun make-constant-ast (value &key origin (policy *policy*))
  (make-instance 'constant-ast
    :origin origin :policy policy
    :value value))

(cleavir-io:define-save-info constant-ast
  (:value value))

(define-children constant-ast ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class representing a lexical variable

(defclass lexical-variable (ast)
  (;; Only used for debugging purposes.
   (%name :initarg :name :reader name)))

(defun make-lexical-variable (name &key origin (policy *policy*))
  (make-instance 'lexical-variable
    :origin origin :policy policy
    :name name))

(cleavir-io:define-save-info lexical-variable
    (:name name))

(define-children lexical-variable ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEXICAL-AST.
;;;
;;; A LEXICAL-AST represents a reference to a lexical variable.

(defclass lexical-ast (one-value-ast-mixin side-effect-free-ast-mixin ast)
  ((%lexical-variable :initarg :lexical-variable :reader lexical-variable)))

(defun make-lexical-ast (lexical-variable &key origin (policy *policy*))
  (make-instance 'lexical-ast
    :origin origin :policy policy
    :lexical-variable lexical-variable))

(cleavir-io:define-save-info lexical-ast
  (:lexical-variable lexical-variable))

(define-children lexical-ast ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SYMBOL-VALUE-AST.
;;;
;;; This AST is generated from a reference to a special variable.

(defclass symbol-value-ast (one-value-ast-mixin side-effect-free-ast-mixin ast)
  ((%symbol-ast :initarg :symbol-ast :reader symbol-ast)))

(defun make-symbol-value-ast (symbol-ast &key origin (policy *policy*))
  (make-instance 'symbol-value-ast
    :origin origin :policy policy
    :symbol-ast symbol-ast))

(cleavir-io:define-save-info symbol-value-ast
  (:symbol-ast symbol-ast))

(define-children symbol-value-ast (symbol-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CONSTANT-SYMBOL-VALUE-AST.

(defclass constant-symbol-value-ast (one-value-ast-mixin side-effect-free-ast-mixin ast)
  ((%name :initarg :name :reader name)))

(defun make-constant-symbol-value-ast (name &key origin (policy *policy*))
  (make-instance 'constant-symbol-value-ast
    :origin origin :policy policy
    :name name))

(cleavir-io:define-save-info constant-symbol-value-ast
  (:name name))

(define-children constant-symbol-value-ast ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SET-SYMBOL-VALUE-AST.
;;;
;;; This AST is generated from an assignment to a special variable.

(defclass set-symbol-value-ast (no-value-ast-mixin ast)
  ((%symbol-ast :initarg :symbol-ast :reader symbol-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-set-symbol-value-ast (symbol-ast value-ast &key origin (policy *policy*))
  (make-instance 'set-symbol-value-ast
    :origin origin :policy policy
    :symbol-ast symbol-ast
    :value-ast value-ast))

(cleavir-io:define-save-info set-symbol-value-ast
  (:symbol-ast symbol-ast)
  (:value-ast value-ast))

(define-children set-symbol-value-ast (symbol-ast value-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SET-CONSTANT-SYMBOL-VALUE-AST.

(defclass set-constant-symbol-value-ast (no-value-ast-mixin ast)
  ((%name :initarg :name :reader name)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-set-constant-symbol-value-ast (name value-ast &key origin (policy *policy*))
  (make-instance 'set-constant-symbol-value-ast
    :origin origin :policy policy
    :name name
    :value-ast value-ast))

(cleavir-io:define-save-info set-constant-symbol-value-ast
  (:name name)
  (:value-ast value-ast))

(define-children set-constant-symbol-value-ast (value-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FDEFINITION-AST.
;;;
;;; This AST is generated from a reference to a global function.

(defclass fdefinition-ast (one-value-ast-mixin side-effect-free-ast-mixin ast)
  (;; This slot contains an AST that produces the function name.
   (%name-ast :initarg :name-ast :reader name-ast)
   (%attributes :initarg :attributes :reader attributes
                :initform (cleavir-attributes:default-attributes))))

(defun make-fdefinition-ast (name-ast
                             &key origin (policy *policy*)
                               (attributes
                                (cleavir-attributes:default-attributes)))
  (make-instance 'fdefinition-ast
    :origin origin :policy policy :attributes attributes
    :name-ast name-ast))

(cleavir-io:define-save-info fdefinition-ast
  (:name-ast name-ast)
  (:attributes attributes))

(define-children fdefinition-ast (name-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CONSTANT-FDEFINITION-AST.
;;;
;;; This AST is not yet generated from a reference to a global
;;; function, but migration to this AST is suggested.

(defclass constant-fdefinition-ast
    (one-value-ast-mixin side-effect-free-ast-mixin ast)
  (;; This slot contains the name of the function
   (%name :initarg :name :reader name)
   (%attributes :initarg :attributes :reader attributes
                :initform (cleavir-attributes:default-attributes))))

(defun make-constant-fdefinition-ast (name
                                      &key origin (policy *policy*)
                                        (attributes
                                         (cleavir-attributes:default-attributes)))
  (make-instance 'constant-fdefinition-ast
    :origin origin :policy policy
    :name name :attributes attributes))

(cleavir-io:define-save-info constant-fdefinition-ast
  (:name name)
  (:attributes attributes))

(define-children constant-fdefinition-ast ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CALL-AST.
;;;
;;; A CALL-AST represents a function call.

(defclass call-ast (ast)
  ((%callee-ast :initarg :callee-ast :reader callee-ast)
   (%argument-asts :initarg :argument-asts :reader argument-asts)
   (%inline :initarg :inline :initform nil :reader inline-declaration)))

(defun make-call-ast (callee-ast argument-asts
                      &key origin inline (policy *policy*))
  (make-instance 'call-ast
    :origin origin :policy policy
    :callee-ast callee-ast
    :inline inline
    :argument-asts argument-asts))

(cleavir-io:define-save-info call-ast
  (:callee-ast callee-ast)
  (:argument-asts argument-asts)
  (:inline inline-declaration))

(define-children call-ast (callee-ast . argument-asts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FUNCTION-AST.
;;;
;;; A function AST represents an explicit lambda expression, but also
;;; implicit lambda expressions such as the ones found in FLET and
;;; LABELS.
;;;
;;; The lambda list is not a normal lambda list.  It has the following
;;; form:
;;; ([r1 .. rl [&optional o1 ..om] [&rest r] [&key k1 .. kn &allow-other-keys]]])
;;;
;;; where:
;;;
;;;   - Each ri is a LEXICAL-VARIABLE.
;;;
;;;   - r is a LEXICAL-VARIABLE.
;;;
;;;   - Each oi is a list of two LEXICAL-VARIABLEs.  The second of the
;;;     two conceptually contains a Boolean value indicating whether
;;;     the first one contains a value supplied by the caller.
;;;
;;;   - Each ki is a list of a symbol and two LEXICAL-VARIABLEs.  The
;;;     symbol is the keyword-name that a caller must supply in order
;;;     to pass the corresponding argument.  The second of the two
;;;     LEXICAL-VARIABLEs conceptually contains a Boolean value indicating
;;;     whether the first LEXICAL-VARIABLE contains a value supplied by the
;;;     caller.
;;;
;;; The LEXICAL-VARIABLEs in the lambda list are potentially unrelated to
;;; the variables that were given in the original lambda expression,
;;; and they are LEXICAL-VARIABLEs independently of whether the
;;; corresponding variable that was given in the original lambda
;;; expression is a lexical variable or a special variable.
;;;
;;; The body of the FUNCTION-AST must contain code that tests the
;;; second of the two LEXICAL-VARIABLEs and initializes variables if
;;; needed.  The if the second LEXICAL-VARIABLE in any oi contains FALSE,
;;; then the code in the body is not allowed to test the second
;;; LEXICAL-VARIABLEs of any of the ki because they may not be set
;;; correctly (conceptually, they all have the value FALSE then).

(defclass function-ast (one-value-ast-mixin side-effect-free-ast-mixin ast)
  ((%lambda-list :initarg :lambda-list :reader lambda-list)
   (%body-ast :initarg :body-ast :reader body-ast)
   ;; An alist from lexical VARIABLEs to lists of pertinent declaration specifiers.
   ;; Since SPECIAL is otherwise handled, these are for optimization use only
   ;; and may be discarded at will.
   (%bound-declarations :initarg :bound-declarations :initform nil
                        :reader bound-declarations)
   ;; These three are intended for debugging/introspection.
   (%name :initarg :name :initform nil :accessor name)
   (%docstring :initarg :docstring :initform nil :reader docstring)
   (%original-lambda-list :initarg :original-lambda-list :initform nil
                          :reader original-lambda-list)
   (%attributes :initarg :attributes :reader attributes
                :initform (cleavir-attributes:default-attributes))))

(defun make-function-ast (body-ast lambda-list
                          &key name docstring original-lambda-list
                            bound-declarations
                            origin (policy *policy*)
                            (attributes
                             (cleavir-attributes:default-attributes)))
  (make-instance 'function-ast
    :origin origin :policy policy
    :name name :docstring docstring
    :original-lambda-list original-lambda-list
    :bound-declarations bound-declarations
    :body-ast body-ast
    :lambda-list lambda-list
    :attributes attributes))

(cleavir-io:define-save-info function-ast
  (:lambda-list lambda-list)
  (:body-ast body-ast)
  (:name name) (:docstring docstring)
  (:bound-declarations bound-declarations)
  (:original-lambda-list original-lambda-list)
  (:attributes attributes))

(defmethod children append ((ast function-ast))
  (list* (body-ast ast)
         (loop for entry in (lambda-list ast)
               append (cond ((symbolp entry)
                             '())
                            ((consp entry)
                             (if (= (length entry) 2)
                                 entry
                                 (cdr entry)))
                            (t
                             (list entry))))))

(defmethod map-children progn (function (ast function-ast))
  (funcall function (body-ast ast))
  (dolist (entry (lambda-list ast))
    (cond ((symbolp entry))
          ((consp entry)
           (if (= (length entry) 2)
               (mapc function entry)
               (mapc function (cdr entry))))
          (t (funcall function entry)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TOP-LEVEL-FUNCTION-AST.
;;;
;;; This AST is a subclass of FUNCTION-AST.  It is used when an AST is
;;; transformed by hoisting all the LOAD-TIME-VALUE-ASTs in the tree
;;; by turning them into LEXIAL-ASTs that are also required parameters
;;; of the TOP-LEVEL-FUNCTION-AST.
;;;
;;; This AST class supplies a slot that contains a list of the forms
;;; that were contained in the LOAD-TIME-VALUE-ASTs.  In order to
;;; evaluate the original AST, the transformed AST must be called with
;;; the values of those forms as arguments.

(defclass top-level-function-ast (function-ast)
  ((%forms :initarg :forms :reader forms)))

(defun make-top-level-function-ast (body-ast lambda-list forms
                                    &key origin (policy *policy*))
  (make-instance 'top-level-function-ast
    :origin origin :policy policy
    :body-ast body-ast
    :lambda-list lambda-list
    :forms forms))

(cleavir-io:define-save-info top-level-function-ast
    (:forms forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PRIMOP-AST.
;;;
;;; A PRIMOP-AST represents the invocation of a primitive operator.
;;; See the cleavir-primop system for more information.

(defclass primop-ast (ast)
  ((%info :initarg :info :reader info)
   (%argument-asts :initarg :argument-asts :reader argument-asts)
   (%attributes :initarg :attributes :reader attributes
                :initform (cleavir-attributes:default-attributes))))

(cleavir-io:define-save-info primop-ast
    (:info info)
  (:argument-asts argument-asts)
  (:attributes attributes))

(define-children primop-ast argument-asts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PROGN-AST.

(defclass progn-ast (ast)
  ((%form-asts :initarg :form-asts :reader form-asts)))

(defun make-progn-ast (form-asts &key origin (policy *policy*))
  (make-instance 'progn-ast
    :origin origin :policy policy
    :form-asts form-asts))

(cleavir-io:define-save-info progn-ast
  (:form-asts form-asts))

(define-children progn-ast form-asts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BLOCK-AST.

(defclass block-ast (ast)
  ((%body-ast :initarg :body-ast :accessor body-ast)
   ;; Original name of the block, for debugging etc.
   (%name :initarg :name :reader name)))

(defun make-block-ast (body-ast &key name origin (policy *policy*))
  (make-instance 'block-ast
    :origin origin :policy policy :name name
    :body-ast body-ast))

(cleavir-io:define-save-info block-ast
  (:name name)
  (:body-ast body-ast))

(define-children block-ast (body-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RETURN-FROM-AST.

(defclass return-from-ast (ast)
  ((%block-ast :initarg :block-ast :reader block-ast)
   (%form-ast :initarg :form-ast :reader form-ast)))

(defun make-return-from-ast (block-ast form-ast &key origin (policy *policy*))
  (make-instance 'return-from-ast
    :origin origin :policy policy
    :block-ast block-ast
    :form-ast form-ast))

(cleavir-io:define-save-info return-from-ast
  (:block-ast block-ast)
  (:form-ast form-ast))

(define-children return-from-ast (form-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SETQ-AST.

(defclass setq-ast (ast)
  ((%lexical-variable :initarg :lexical-variable :reader lexical-variable)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-setq-ast (lexical-variable value-ast &key origin (policy *policy*))
  (make-instance 'setq-ast
    :origin origin :policy policy
    :lexical-variable lexical-variable
    :value-ast value-ast))

(cleavir-io:define-save-info setq-ast
  (:lexical-variable lexical-variable)
  (:value-ast value-ast))

(define-children setq-ast (value-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEXICAL-BIND-AST.
;;;
;;; This AST represents the binding of a lexical variable.

(defclass lexical-bind-ast (ast)
  ((%lexical-variable :initarg :lexical-variable :reader lexical-variable)
   (%value-ast :initarg :value-ast :reader value-ast)
   (%ignore :initarg :ignore :reader ignore)))

(defun make-lexical-bind-ast (lexical-variable value-ast &key ignore origin (policy *policy*))
  (make-instance 'lexical-bind-ast
    :origin origin :policy policy
    :lexical-variable lexical-variable
    :value-ast value-ast
    :ignore ignore))

(cleavir-io:define-save-info lexical-bind-ast
  (:lexical-variable lexical-variable)
  (:value-ast value-ast)
  (:ignore ignore))

(define-children lexical-bind-ast (lexical-variable value-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DYNAMIC-BIND-AST
;;;
;;; This AST represents the binding of a special variable.
;;;

(defclass dynamic-bind-ast (ast)
  ((%name :initarg :name-ast :reader name-ast)
   (%value :initarg :value-ast :reader value-ast)
   (%body :initarg :body-ast :reader body-ast)))

(cleavir-io:define-save-info dynamic-bind-ast
    (:name-ast name-ast)
  (:value-ast value-ast)
  (:body-ast body-ast))

(define-children dynamic-bind-ast
    (name-ast value-ast body-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TAG-AST.
;;; The TAG-AST includes the tag itself, and also the code following it
;;; that is not after another tag.

(defclass tag-ast (ast)
  ((%name :initarg :name :reader name)
   (%body-ast :initarg :body-ast :accessor body-ast)))

(defun make-tag-ast (name &key origin (policy *policy*))
  (make-instance 'tag-ast
    :origin origin :policy policy
    :name name))

(cleavir-io:define-save-info tag-ast
  (:name name) (:body-ast body-ast))

(define-children tag-ast (body-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TAGBODY-AST.

(defclass tagbody-ast (no-value-ast-mixin ast)
  ((%prefix-ast :initarg :prefix-ast :reader prefix-ast)
   ;; A proper list of TAG-ASTs.
   (%item-asts :initarg :item-asts :reader item-asts)))

(defun make-tagbody-ast (prefix-ast item-asts &key origin (policy *policy*))
  (make-instance 'tagbody-ast
    :origin origin :policy policy
    :prefix-ast prefix-ast :item-asts item-asts))

(cleavir-io:define-save-info tagbody-ast
    (:prefix-ast prefix-ast) (:item-asts item-asts))

(define-children tagbody-ast (prefix-ast . item-asts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GO-AST.

(defclass go-ast (ast)
  ((%tag-ast :initarg :tag-ast :reader tag-ast)))

(defun make-go-ast (tag-ast &key origin (policy *policy*))
  (make-instance 'go-ast
    :origin origin :policy policy
    :tag-ast tag-ast))

(cleavir-io:define-save-info go-ast
  (:tag-ast tag-ast))

(define-children go-ast ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class UNWIND-PROTECT-AST.

(defclass unwind-protect-ast (ast)
  ((%body :initarg :body-ast :reader body-ast)
   ;; This will be a FUNCTION-AST - a thunk (no arguments).
   (%cleanup :initarg :cleanup-ast :reader cleanup-ast)))

(cleavir-io:define-save-info unwind-protect-ast
    (:body-ast body-ast) (:cleanup-ast cleanup-ast))

(define-children unwind-protect-ast (body-ast cleanup-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class THE-AST.
;;;
;;; This AST can be generated by from the THE special operator, but
;;; also implicitly from type declarations and assignments to
;;; variables with type declarations.

(defclass the-ast (ast)
  ((%form-ast :initarg :form-ast :reader form-ast)
   ;; A VALUES ctype.
   (%ctype :initarg :ctype :reader ctype)
   ;; This slot holds either a function which checks the FORM-AST,
   ;; :TRUSTED if we want to treat this as trusted type assertion with
   ;; no check needed, or NIL if the declaration is untrusted but not
   ;; necessarily checked. See BIR/instructions.lisp for the canonical
   ;; definition.
   (%type-check-function-ast :initarg :type-check-function-ast :reader type-check-function-ast)))

(defun make-the-ast (form-ast ctype type-check-function-ast &key origin (policy *policy*))
  (make-instance 'the-ast
    :origin origin :policy policy
    :form-ast form-ast :ctype ctype
    :type-check-function-ast type-check-function-ast))

(cleavir-io:define-save-info the-ast
  (:form-ast form-ast)
  (:ctype ctype)
  (:type-check-function-ast type-check-function-ast))

(defmethod children append ((ast the-ast))
  (let ((form-ast (form-ast ast))
        (type-check-function-ast (type-check-function-ast ast)))
    (if (symbolp type-check-function-ast)
        (list form-ast)
        (list form-ast type-check-function-ast))))

(defmethod map-children progn (function (ast the-ast))
  (let ((form-ast (form-ast ast))
        (type-check-function-ast (type-check-function-ast ast)))
    (cond ((symbolp type-check-function-ast)
           (funcall function form-ast))
          (t
           (funcall function form-ast)
           (funcall function type-check-function-ast)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TYPEQ-AST.
;;;
;;; This AST can be thought of as a translation to an AST of a
;;; hypothetical special form (TYPEQ <form> <type-specifier>) which is
;;; like the function TYPEP, except that the type specifier is not
;;; evaluated.
;;;
;;; However, this AST can only occur in the conditional position of an
;;; IF-AST.

(defclass typeq-ast (boolean-ast-mixin ast)
  ((%test-ctype :initarg :test-ctype :reader test-ctype)
   (%form-ast :initarg :form-ast :reader form-ast)))

(defun make-typeq-ast (form-ast test-ctype &key origin (policy *policy*))
  (make-instance 'typeq-ast
    :origin origin :policy policy
    :form-ast form-ast
    :test-ctype test-ctype))

(cleavir-io:define-save-info typeq-ast
  (:test-ctype test-ctype)
  (:form-ast form-ast))

(define-children typeq-ast (form-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LOAD-TIME-VALUE-AST.
;;;
;;; This AST corresponds directly to the LOAD-TIME-VALUE special
;;; operator.  It has a single child and it produces a single value.
;;;
;;; The optional argument READ-ONLY-P is not a child of the AST
;;; because it can only be a Boolean which is not evaluated, so we
;;; know at AST creation time whether it is true or false.

(defclass load-time-value-ast (one-value-ast-mixin ast)
  ((%form :initarg :form :reader form)
   (%read-only-p :initarg :read-only-p :reader read-only-p)))

(defun make-load-time-value-ast (form &optional read-only-p &key origin (policy *policy*))
  (make-instance 'load-time-value-ast
    :origin origin :policy policy
    :form form
    :read-only-p read-only-p))

;;; Even though READ-ONLY-P is not a child of the AST, it needs to be
;;; saved when the AST is saved.
(cleavir-io:define-save-info load-time-value-ast
  (:form form)
  (:read-only-p read-only-p))

(define-children load-time-value-ast ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IF-AST.
;;;
;;; This AST corresponds directly to the IF special operator.  It
;;; produces as many values as the AST in the THEN-AST or ELSE-AST
;;; produces, according to the value of the TEST AST.

(defclass if-ast (ast)
  ((%test-ast :initarg :test-ast :reader test-ast)
   (%then-ast :initarg :then-ast :reader then-ast)
   (%else-ast :initarg :else-ast :reader else-ast)))

(defun make-if-ast (test-ast then-ast else-ast &key origin (policy *policy*))
  (make-instance 'if-ast
    :origin origin :policy policy
    :test-ast test-ast
    :then-ast then-ast
    :else-ast else-ast))

(cleavir-io:define-save-info if-ast
  (:test-ast test-ast)
  (:then-ast then-ast)
  (:else-ast else-ast))

(define-children if-ast (test-ast then-ast else-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BRANCH-AST.
;;;
;;; This class is a generalization of IF-AST. Based on the TEST-AST,
;;; one of the zero or more BRANCH-ASTs, or else the DEFAULT-AST,
;;; will be evaluated and its values returned.
;;;
;;; This AST can be used for example in the implementation of fast
;;; CASE or TYPECASE operations.

(defclass branch-ast (ast)
  ((%test-ast :initarg :test-ast :reader test-ast)
   (%branch-asts :initarg :branch-asts :reader branch-asts)
   (%default-ast :initarg :default-ast :reader default-ast)))

(defun make-branch-ast (test-ast branch-asts default-ast
                        &key origin (policy *policy*))
  (make-instance 'branch-ast
    :origin origin :policy policy
    :test-ast test-ast
    :branch-asts branch-asts :default-ast default-ast))

(cleavir-io:define-save-info branch-ast
  (:test-ast test-ast)
  (:branch-asts branch-asts)
  (:default-ast default-ast))

(define-children branch-ast (test-ast default-ast . branch-asts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MULTIPLE-VALUE-CALL-AST.

(defclass multiple-value-call-ast (ast)
  ((%function-form-ast :initarg :function-form-ast :reader function-form-ast)
   (%form-asts :initarg :form-asts :reader form-asts)))

(defun make-multiple-value-call-ast
    (function-form-ast form-asts &key origin (policy *policy*))
  (make-instance 'multiple-value-call-ast
    :origin origin :policy policy
    :function-form-ast function-form-ast :form-asts form-asts))

(cleavir-io:define-save-info multiple-value-call-ast
  (:function-form-ast function-form-ast)
  (:form-asts form-asts))

(define-children multiple-value-call-ast (function-form-ast . form-asts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VALUES-AST.
;;;
;;; This corresponds directly to CLEAVIR-PRIMOP:VALUES,
;;; and CL:VALUES through it.

(defclass values-ast (ast)
  ((%argument-asts :initarg :argument-asts :reader argument-asts)))

(defun make-values-ast
    (argument-asts &key origin (policy *policy*))
  (make-instance 'values-ast
    :origin origin :policy policy
    :argument-asts argument-asts))

(cleavir-io:define-save-info values-ast
  (:argument-asts argument-asts))

(define-children values-ast argument-asts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MULTIPLE-VALUE-PROG1-AST.

(defclass multiple-value-prog1-ast (ast)
  ((%first-form-ast :initarg :first-form-ast :reader first-form-ast)
   ;; A list of ASTs
   (%form-asts :initarg :form-asts :reader form-asts)))

(defun make-multiple-value-prog1-ast (first-form-ast form-asts &key origin (policy *policy*))
  (make-instance 'multiple-value-prog1-ast
    :origin origin :policy policy
    :first-form-ast first-form-ast
    :form-asts form-asts))

(cleavir-io:define-save-info multiple-value-prog1-ast
  (:first-form-ast first-form-ast)
  (:form-asts form-asts))

(define-children multiple-value-prog1-ast (first-form-ast . form-asts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DYNAMIC-ALLOCATION-AST
;;;
;;; This AST is used to translate DYNAMIC-EXTENT declarations.
;;; Any allocation done by its form-ast may be done dynamically,
;;; i.e. with stack discipline. This means that the consequences
;;; are undefined if any value allocated by the form-ast escapes
;;; the local function.
;;; Note that this loses information from DYNAMIC-EXTENT, which
;;; does not allow escape from the form with the declaration.

(defclass dynamic-allocation-ast (one-value-ast-mixin ast)
  ((%form-ast :initarg :form-ast :reader form-ast)))

(defun make-dynamic-allocation-ast (form-ast &key origin (policy *policy*))
  (make-instance 'dynamic-allocation-ast
    :origin origin :policy policy
    :form-ast form-ast))

(cleavir-io:define-save-info dynamic-allocation-ast
  (:form-ast form-ast))

(define-children dynamic-allocation-ast (form-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class UNREACHABLE-AST.
;;;
;;; This AST indicates an unreachable control point.
;;; Control that leads inevitably from or to this AST is
;;; declared to be impossible.

(defclass unreachable-ast (ast) ())

(defun make-unreachable-ast (&key origin (policy *policy*))
  (make-instance 'unreachable-ast :origin origin :policy policy))

(define-children unreachable-ast ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class EQ-AST.
;;;
;;; This AST can be used to to test whether two objects are identical.
;;; It has two children.  This AST can only appear in the TEST
;;; position of an IF-AST.

(defclass eq-ast (boolean-ast-mixin ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-eq-ast (arg1-ast arg2-ast &key origin (policy *policy*))
  (make-instance 'eq-ast
    :origin origin :policy policy
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info eq-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(define-children eq-ast (arg1-ast arg2-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CASE-AST.
;;;
;;; This AST can be used to select an execution path by
;;; comparing a given object against a fixed set of immediates.
;;; COMPAREES is a sequence of sequences of objects.
;;; If the primary value returned by the ARG-AST is EQ to one of
;;; the objects in the nth sequence, the nth branch is taken;
;;; if the value doesn't match any immediate the default branch
;;; is taken instead.
;;; This AST can only appear in the TEST position of a BRANCH-AST.

(defclass case-ast (multiway-ast-mixin ast)
  ((%arg-ast :initarg :arg-ast :reader arg-ast)
   (%comparees :initarg :comparees :reader comparees)))

(defun make-case-ast (arg-ast comparees &key origin (policy *policy*))
  (make-instance 'case-ast
    :origin origin :policy policy
    :arg-ast arg-ast :comparees comparees))

(cleavir-io:define-save-info case-ast
  (:arg-ast arg-ast)
  (:comparees comparees))

(define-children case-ast (arg-ast))
