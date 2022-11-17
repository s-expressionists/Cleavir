(in-package #:cleavir-bir)

;;; This instruction creates a first-class object from a FUNCTION.
(defclass enclose (no-input one-output instruction)
  ((%code :initarg :code :reader code
          :type function
          :documentation "The BIR function representing the code of the closure that will be created.")
   (%extent :initarg :extent :accessor extent
            :initform :indefinite
            :type (member :dynamic :indefinite)
            :documentation "The extent of the closure created by this instruction."))
  (:documentation "Instruction representing runtime creation of a first-class function. No inputs or outputs.

See CODE
See EXTENT"))

(defclass unreachable (no-input no-output terminator0)
  ()
  (:documentation "Terminator. Represents the fact that control never reaches this point. No inputs or outputs."))

;; FIXME: Remove?
(defclass nop (no-input no-output instruction)
  ()
  (:documentation "Instruction. No effect. No inputs or outputs."))

(defclass accessvar (instruction)
  ()
  (:documentation "Abstract instruction dealing with a variable."))

(defclass writevar (one-input one-output accessvar)
  ()
  (:documentation "Instruction representing writing a value to a VARIABLE. The single input is the value to write, and the single output is the VARIABLE to write to."))

(defclass readvar (one-input one-output accessvar)
  ()
  (:documentation "Instruction representing reading the value of a VARIABLE. The single input is the VARIABLE to read from, and the single output is the value read."))

;;; Constants

(defclass constant-reference (one-input one-output instruction)
  ()
  (:documentation "Instruction representing the use of a constant. The single input is a CONSTANT and the single output is its value."))

;;; Retrieve the function bound to a constant.
(defclass constant-fdefinition (one-input one-output instruction)
  ()
  (:documentation "Instruction representing the lookup of a global function with a known name. The single input is a CONSTANT whose value must be a function name, and the single output is the function looked up."))

(defclass load-time-value-reference (one-input one-output instruction)
  ()
  (:documentation "Instruction representing the lookup of a load-time-value. The single input is a LOAD-TIME-VALUE, and the single output is the computed value."))

;;; Retrieve the value bound to a known symbol.
(defclass constant-symbol-value (one-input one-output instruction)
  ()
  (:documentation "Instruction representing the lookup of the value of a symbol with a known name. The single input is a CONSTANT whose value must be a symbol, and the single output is its value."))

(defclass set-constant-symbol-value (no-output instruction)
  ()
  (:documentation "Instruction representing the modification of the value of a symbol with a known name. The first input is a CONSTANT whose value must be a symbol, and the second is the value the symbol's value will be changed to. No output."))

(defclass primop (instruction)
  ((%info :initarg :info :reader info
          :type primop-info:info))
  (:documentation "Instruction with similar semantics to a call, but which is specially handled by the backend rather than becoming a normal function call. Inputs and outputs depend on the particular primop."))

(defmethod attributes ((instruction primop))
  (primop-info:attributes (info instruction)))

(defclass abstract-call (one-output instruction)
  ()
  (:documentation "Abstract instruction. The superclass for all instructions that represent a function call. The one output is the values returned by the call.

See CALLEE
See CALL
See MV-CALL"))

(defgeneric callee (instruction)
  (:documentation "Given an ABSTRACT-CALL, return the function being called. For a local call, this will be the FUNCTION. Otherwise it is a VALUE representing the function."))

(defclass call (abstract-call)
  ()
  (:documentation "Instruction representing a normal call, to a function that Cleavir does not know to be in this MODULE (probably because it really isn't). The first input is the CALLEE, and the subsequent inputs are the arguments. Because this is a normal call, only the primary values are to be taken from each input."))
(defmethod callee ((i call)) (first (inputs i)))

(defclass mv-call (abstract-call)
  ()
  (:documentation "Instruction representing a multiple-value call to a function that Cleavir does not know to be in this MODULE (probably because it really isn't). The first input is the CALLEE, and the subsequent inputs are the arguments."))
(defmethod callee ((i mv-call)) (first (inputs i)))

;;; convenience method
(defmethod attributes ((instruction abstract-call))
  (attributes (callee instruction)))

;;; A local call is a legal call to a function within the same
;;; module. Therefore, the first input is actually a
;;; FUNCTION. Importantly, illegal calls (i.e. ones whose arguments
;;; are not compatible with the lambda list of the callee) are not
;;; classified as local calls, both so that they can reuse the same
;;; runtime mechanism for argument count errors that normal calls from
;;; outside a module use, and so that we can make the assumption that
;;; local calls have compatible arguments with the lambda list of the
;;; callee.
(defclass abstract-local-call (abstract-call)
  ()
  (:documentation "Abstract instruction representing a local call, i.e. a call to a FUNCTION in this MODULE. The first input is this callee.
Importantly, illegal calls, i.e. ones whose arguments are not compatible with the lambda list of the callee, are not classified as local calls. This so that the presumed runtime mechanism for argument count errors can be used. MV-LOCAL-CALLs do not have a known argument count, and therefore could end up being illegal at runtime, but otherwise this also lets us assume local calls are legal."))

(defclass local-call (abstract-local-call)
  ()
  (:documentation "A normal local call. The primary values of the second and further inputs are the arguments."))
(defmethod callee ((i local-call))
  (let ((function (first (inputs i))))
    (check-type function function)
    function))

(defclass mv-local-call (abstract-local-call)
  ()
  (:documentation "A multiple-value local call. The second and further inputs are the arguments."))
(defmethod callee ((i mv-local-call)) (first (inputs i)))

(defclass returni (one-input no-output terminator0)
  ()
  (:documentation "Terminator, representing the return of the one input from this FUNCTION."))

(defclass values-save
    (dynamic-environment one-input one-output terminator1)
  ()
  (:documentation "Terminator and dynamic environment, representing saving some computed values temporarily while other computations complete. The saved values are only valid within this dynamic environment. Used in the representation of cl:multiple-value-call and cl:multiple-value-prog1. The input and output are the values saved.

See VALUES-SAVE
See VALUES-COLLECT"))

(defclass fixed-values-save (values-save)
  ((%nvalues :initarg :nvalues :reader nvalues))
  (:documentation "Specialization of VALUES-SAVE used when the number of values is known. Transformed from VALUES-SAVE by meta evaluation."))

;;; FIXME: Enforce that the input must be from a values-save, maybe
(defclass values-restore (one-input one-output instruction)
  ()
  (:documentation "Instruction representing the restoration of values saved by VALUES-SAVE. The input and output are these values.

See VALUES-SAVE"))

(defclass values-collect (dynamic-environment one-output terminator1)
  ()
  (:documentation "Terminator and dynamic environment representing the collection of many values together. The append values are only valid within this dynamic environment. Used in the representation of cl:multiple-value-call with multiple argument forms. The inputs are the values to collect, and the output is the result of their appendage.

See VALUES-SAVE"))

;;; FIXME: The one input should be the bir:function for the cleanup - we never need
;;; a closure here.
(defclass unwind-protect (dynamic-environment one-input no-output
                          terminator1)
  ()
  (:documentation "Terminator and dynamic environment representing unwind protection. Nonlocal exits from within this dynamic environment to any outside dynamic environment are interrupted by the execution of a cleanup function, which is the sole input to this instruction. No outputs."))

(defclass come-from (no-input no-output lexical ssa
                     dynamic-environment terminator)
  ((%unwinds :initarg :unwinds :accessor unwinds
             :initform (set:empty-set)
             :type set:set
             :documentation "The set of UNWINDs targeting this COME-FROM."))
  (:documentation "Terminator and dynamic environment representing a possible nonlocal entry, as from cl:block or cl:tagbody. As a VALUE, represents the runtime representation of the continuation. The first of the NEXT IBLOCKs is the normal child continuation, i.e. the body of the cl:block or the prefix of the cl:tagbody, and the subsequent IBLOCKs are the possible destinations for an UNWIND to this COME-FROM. No inputs or outputs.

See UNWIND"))

(defclass leti (writevar)
  ()
  (:documentation "Instruction representing the initial binding of a variable.

See WRITEVAR"))

(defclass dynamic-leti (leti terminator1 dynamic-environment)
  ()
  (:documentation "Terminator and dynamic environment representing the initial binding of a variable that has dynamic extent. The variable can only validly be used within this dynamic environment.

See LETI"))

;;; Dynamic binding. Inputs are the symbol and the new value.
(defclass bind (dynamic-environment no-output terminator1)
  ()
  (:documentation "Terminator and dynamic environment representing the binding of a dynamic variable. Within this dynamic environment, the symbol has this value (unless there is a more recent binding, of course). The first input is the symbol being bound, and the second input its new value. No outputs."))

(defclass unwind (terminator0)
  ((%come-from :initarg :come-from :reader come-from
               :type come-from)
   (%destination :initarg :destination :reader destination
                 :type iblock))
  (:documentation "Terminator representing a nonlocal control transfer, i.e. a control transfer to another function. Inputs are passed to the destination."))

(defclass jump (terminator1)
  ()
  (:documentation "Terminator representing an unconditional local control transfer. Inputs are passed to the single NEXT IBLOCK."))

(defmethod unwindp ((instruction jump))
  "Returns true iff the dynamic environment of the INSTRUCTION's IBLOCK is distinct from the dynamic environment the instruction will transfer control to."
  (not (eq (dynamic-environment (iblock instruction))
           (dynamic-environment (first (next instruction))))))

(defclass ifi (one-input no-output terminator)
  ()
  (:documentation "Terminator representing a branch. If the sole input is NIL, control is transferred to the second NEXT, and otherwise to the first. This is the canonical way to branch in Cleavir, which optimization passes know how to deal with."))

(defclass conditional-test (one-output instruction)
  ()
  (:documentation "Abstract instruction representing a computation whose value is guaranteed to be used only as the input to an IFI instruction. (Note that non-CONDITIONAL-TESTs are also allowed to output to an IFI instruction.) The reason for this constraint is that these can usually be specially treated by a backend."))

(defclass eq-test (conditional-test)
  ()
  (:documentation "Instruction representing a cl:eq test. The sole output is true if the two inputs are cl:eq, and otherwise false."))
(defclass typeq-test (one-input conditional-test)
  ((%test-ctype :initarg :test-ctype :reader test-ctype))
  (:documentation "Instruction representing a type test, as from cl:typep. The sole output is true if the sole input is of the TEST-CTYPE, and otherwise false."))

(defclass case (one-input no-output terminator)
  ((%comparees :initarg :comparees :reader comparees)))

(defclass fixed-to-multiple (one-output instruction)
  ()
  (:documentation "Instruction representing the aggregation of single values, as by cl:values."))

(defclass thei (one-input one-output instruction)
  ((%asserted-type :initarg :asserted-type :accessor asserted-type)
   ;; This slot holds either:
   ;; * A (BIR) function, that will be called to determine if the input
   ;;   is of the given type, and return it if so.
   ;;   This is intended to be used in high-safety code, or to guard calls
   ;;   to functions that would have bad behavior if given the wrong types.
   ;;   The type declaration is propagated to both asserted and derived types.
   ;; * :TRUSTED, meaning that this will be treated as a trusted type assertion.
   ;;   This is intended to be used in low-safety code, or for the client to
   ;;   communicate information it is completely sure of (e.g. CONS always
   ;;   returns a cons, and this does not need to be checked).
   ;;   The type declaration is propagated to both asserted and derived types.
   ;; * NIL, meaning that this declaration is untrusted.
   ;;   This is intended to be used to translate THE in normal code.
   ;;   The type declaration is propagated to asserted types only.
   (%type-check-function :initarg :type-check-function :initform nil
                         :accessor type-check-function
                         :type (or (member :trusted nil) function)))
  (:documentation "Instruction representing a type assertion on the sole input. The output is identical to the output (this makes the linearity property easy to conserve)."))
