(cl:in-package #:cleavir-cst-to-ast)

(defgeneric convert (client cst environment))

(defgeneric convert-cst (client cst description environment))

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
;;; a description in the form of an DESCRIPTION instance.
;;; That DESCRIPTION instance was obtained as a result of a form such as
;;; (FUNCTION FUNCTION-NAME) by calling DESCRIBE-FUNCTION with
;;; an environment and the FUNCTION-NAME argument.  The function signals
;;; an error if the DESCRIPTION instance is not a
;;; GLOBAL-FUNCTION-DESCRIPTION or a LOCAL-FUNCTION-DESCRIPTION.
;;; Client code can override the default behavior by adding methods to
;;; this function, specialized to the particular client defined by that
;;; client code.

(defgeneric convert-function-reference (client cst description env))

(defgeneric convert-called-function-reference (client cst description env))

(defgeneric items-from-parameter-group (parameter-group))

(defgeneric convert-global-function-reference (client cst description global-env))

(defgeneric convert-special-variable (client cst description global-env))

(defgeneric convert-setq (client var-cst form-cst description env))

(defgeneric convert-setq-special-variable
    (client var-cst form-ast description global-env))

(defgeneric convert-let (client cst environment))

(defgeneric convert-let* (client cst environment))

;; FIXME(paul) Does cst-eval here need two environments or only one?
;; In the trucler branch of cleavir, it had one only (but two in the
;; cleavir-environment package).
;; Assuming we want client and one environment, as Bike suggested, what
;; to do about cst-eval-for-effect (it was passing the same environment
;; for both)?
;; I think it would be fine to just remove the second env.
;;
;; Also, the dispatch-env used to be climbed up to the
;; global-environment in the default implementations, but that seems
;; like it could be wrong here.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function EVAL.
;;;
;;; We define EVAL as a generic function with three arguments: a form
;;; and two environments.  The first environment is the environment in
;;; which the form is to be evaluated.  The second environment is used
;;; for generic dispatch only.
;;;
;;; When some Cleavir tools such as the minimal compiler or the AST
;;; generator are used in a context where a top-level form is
;;; processed by the file compiler, it handles EVAL-WHEN with a
;;; situation of :COMPILE-TOPLEVEL by calling the EVAL generic
;;; function.
;;;
;;; For the discussion that follows, recall that we use the word
;;; "intrinsic" to mean "native", as in the host and the target
;;; environment are the same, and we use the word "extrinsic" to mean
;;; "cross", i.e., the two environments are not the same.
;;;
;;; The reason for calling the Cleavir-specific generic function
;;; rather than just calling CL:EVAL is that CL:EVAL might not be the
;;; right thing to do.
;;;
;;; One problematic case is when the global environment (the so-called
;;; evaluation environment) is side effected at compile time, for
;;; instance as a result of evaluating a DEFUN form at compile time.
;;;
;;;   * When the Cleavir tools in question are used as extrinsic tools
;;;     (i.e., the host system is different from the target system),
;;;     then CL:EVAL would side effect the host environment, whereas
;;;     it is more likely that the target environment should be side
;;;     effected.  For instance, when a target system is bootstrapped
;;;     on a host system, the Cleavir tools might be used to fill up
;;;     the target environment with definitions of standard macros and
;;;     special operators.
;;;
;;;   * A solution that always works would be to handle compile-time
;;;     evaluation by converting the form to an AST and then
;;;     interpreting that AST.  However, then we have the inverse
;;;     problem, namely when the tools are used as intrinsic (native)
;;;     tools, then we might store interpreted functions in the global
;;;     environment.  So, although the solution works, it might result
;;;     in slow code.  In this case, it is more likely that we want to
;;;     call the native EVAL function in order to obtain a compiled
;;;     function.
;;;
;;; Another problematic case with compile-time evaluation is that the
;;; standard treats the body of a LOCALLY, MACROLET, or a
;;; SYMBOL-MACROLET as a top-level form when the FORM itself is a
;;; top-level form.  The HyperSpec has a remark saying "Note that this
;;; implies that the lexical environment in which top level forms are
;;; processed is not necessarily the null lexical environment".  The
;;; problem here arises when the tools are used as intrinsic tools
;;; (i.e., the host and the target environments are the same).  We
;;; can't just call CL:EVAL, because CL:EVAL always evaluates the form
;;; in the null lexical environment.
;;;
;;; Finally, the generic EVAL provides a solution that Cleavir uses
;;; for processing MACROLET forms.  The problem here is that the local
;;; macro definitions should be processed differently in the extrinsic
;;; and the intrinsic case.  In the extrinsic case, the expander
;;; functions should be generated as host functions so that expansion
;;; can take place at compile time.  Cleavir does this by calling the
;;; generic EVAL to process the lambda expressions resulting from
;;; macro definitions.
;;;
;;; When the Cleavir tools are used as extrinsic tools, a reasonable
;;; method on EVAL would be to convert the form to an AST and then
;;; interpreting that AST in the global environment.  It is
;;; reasonable, because the AST presumably already contains all the
;;; results of the influence of the environment on the compilation
;;; process.  Furthermore, any side effect on the global environment
;;; will then happen in the target environment.  Function definitions
;;; that are entered into the target environment are interpreted, of
;;; course, but that is the only option for the extrinsic case anyway.
;;;
;;; For the intrinsic case, the right solution is to convert the form
;;; to an AST using the first environment and then to native code, and
;;; finally executing that native code.  This solution ensures that
;;; function definitions create native code.
;;;
;;; A quick-and-dirty (but incorrect) solution for the intrinsic case
;;; might be to minimally compile (in the sense of "minimal
;;; compilation") the form and then calling CL:EVAL on the result.  It
;;; is incorrect because any declarations present in the environment
;;; are lost when CL:EVAL is called.

(defgeneric eval (form environment dispatch-environment))

(defmethod eval (form environment dispatch-environment)
  (eval form environment
        (trucler:global-environment nil dispatch-environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CST-EVAL.
;;;
;;; This version of EVAL takes a concrete syntax tree rather than a
;;; Common Lisp form.  Also, this version of eval has an initial
;;; parameter CLIENT, so that we can evaluate CSTs differently
;;; according to the particular implementation we have.
(defgeneric cst-eval (client cst environment dispatch-environment))

(defmethod cst-eval (client cst environment dispatch-environment)
  (cst-eval client cst environment
            (trucler:global-environment client dispatch-environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CST-EVAL-FOR-EFFECT.
;;;
;;; This generic function is called in order on every CST that is
;;; evaluated for compile-time-too mode. The default method just
;;; calls CST-EVAL.
;;; A client could, for example, specialize this function to produce
;;; a "CFASL" file recording only compile time side effects.

(defgeneric cst-eval-for-effect (client cst environment))

(defmethod cst-eval-for-effect (client cst environment)
  (cst-eval client cst environment environment))

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
;;; which will not have specific source information.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ATTRIBUTES.
;;;
;;; FIXME(paul) Add doc?
;;;
;;; FIXME(paul) If this and POLICY take environment, should it go right
;;; after client, to follow Trucler's argument order?
;;; Since they used to be part of cleavir-env, it would seem that they
;;; are closer to Trucler than CST-to-AST, though then again, it might
;;; be confusing to have some CST-to-AST functions with environment
;;; last, some not.

(defgeneric attributes (client description environment))

(defmethod attributes
    (client (description trucler:authentic-function-description) environment)
  (declare (ignore client description environment))
  (cleavir-attributes:default-attributes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function POLICY.
;;;
;;; FIXME(paul) Add doc?
;;;
;;; FIXME(paul) Should cleavir-compilation-policy use
;;; TRUCLER:OPTIMIZE-DESCRIPTIONs since we use them here too?

(defgeneric policy (client description environment))

(defmethod policy
    (client (description trucler:optimize-description) environment)
  (declare (ignore environment))
  (cleavir-policy:compute-policy
   client
   `((speed ,(trucler:speed description))
     (compilation-speed ,(trucler:compilation-speed description))
     (debug ,(trucler:debug description))
     (space ,(trucler:space description))
     (safety ,(trucler:safety description)))))
