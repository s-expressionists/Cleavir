(cl:in-package #:cleavir-cst-to-ast)

(defgeneric convert (client cst environment))

(defgeneric convert-cst (client cst info environment))

(defgeneric convert-special (client head cst environment))

(defgeneric convert-special-binding
    (client variable value-ast next-ast env))

(defgeneric convert-lambda-call (client cst env))

(defgeneric convert-code (client lambda-list body-cst env &key block-name-cst origin))

(defgeneric convert-variable (client cst environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CONVERT-FUNCTION-REFERENCE.
;;;
;;; This generic function converts a reference to a function given by
;;; a description in the form of an INFO instance.  That INFO instance
;;; was obtained as a result of a form such as (FUNCTION
;;; FUNCTION-NAME) by calling FUNCTION-INFO with an environment and
;;; the FUNCTION-NAME argument.  The function signals an error if the
;;; INFO instance is not a GLOBAL-FUNCTION-INFO or a
;;; LOCAL-FUNCTION-INFO.  Client code can override the default
;;; behavior by adding methods to this function, specialized to the
;;; particular client defined by that client code.

(defgeneric convert-function-reference (client cst info env))

(defgeneric convert-called-function-reference (client cst info env))

(defgeneric items-from-parameter-group (parameter-group))

(defgeneric convert-global-function-reference (client cst info global-env))

(defgeneric convert-special-variable (client cst info global-env))

(defgeneric convert-setq (client var-cst form-cst info env))

(defgeneric convert-setq-special-variable
    (client var-cst form-ast info global-env))

(defgeneric convert-let (client cst environment))

(defgeneric convert-let* (client cst environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CST-EVAL-FOR-EFFECT.
;;;
;;; This generic function is called in order on every CST that is
;;; evaluated for compile-time-too mode. The default method just
;;; calls CLEAVIR-ENV:CST-EVAL.
;;; A client could, for example, specialize this function to produce
;;; a "CFASL" file recording only compile time side effects.

(defgeneric cst-eval-for-effect (client cst environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function TYPE-WRAP.
;;;
;;; Given an AST and a values ctype, returns a new AST that incorporates
;;; the information that the AST's value will be of that ctype, in some
;;; client-defined fashion. For example it could execute a type check,
;;; or puts in a type declaration (a the-ast), or it could just return
;;; the AST as-is, ignoring the information.
;;; There is a default method that returns the AST as-is.
;;; ORIGIN is given explicitly because the AST may be a LEXICAL-AST,
;;;
;;; which will not have specific source info.
;;; CONTEXT is an indication of how this type information is arising.
;;; Possible values are:
;;;  * :the. This is a THE form.
;;;  * :variable. This is a variable read. The AST will be exactly
;;;    one value at runtime.
;;;  * :setq. This is the value form in a primitive SETQ or a binding.
;;;    Exactly one value of the AST will be used, but it may have any
;;;    number of values.
;;;  * :argument. This is an argument to a call, and the type
;;;    declaration arises from the type of the function.
;;;    Exactly one value of the AST will be used, but it may have any
;;;    number of values.
;;;  * :return. This is the return value of a call.
;;;
;;; This is provided so that clients do not need to worry about all
;;; values of a form in common cases.
;;; For a context of :VARIABLE, :SETQ, or :ARGUMENT, ctype is a
;;; non-values ctype (the primary), otherwise a values ctype.

(defgeneric type-wrap (client ast ctype context origin environment)
  (:method (client ast ctype context origin environment)
    (declare (ignore client ctype context origin environment))
    ast))
