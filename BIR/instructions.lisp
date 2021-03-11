(in-package #:cleavir-bir)

;;; This instruction creates a first-class object from a FUNCTION.
(defclass enclose (no-input one-output instruction)
  ((%code :initarg :code :reader code
          :type function)
   ;; Indicates the extent of the closure created by this
   ;; instruction.
   (%extent :initarg :extent :accessor extent
            :initform :indefinite
            :type (member :dynamic :indefinite))
   (%derived-type :initform (current-top-function-ctype))))

(defclass unreachable (no-input no-output terminator0) ())

(defclass nop (no-input no-output instruction) ())

;;; Abstract. An instruction dealing with a variable.
(defclass accessvar (instruction) ())

(defclass writevar (one-input one-output accessvar) ())

(defclass readvar (one-input one-output accessvar) ())

;;; Constants

(defclass constant-reference (one-input one-output instruction) ())

(defclass load-time-value-reference (one-input one-output instruction) ())

;;; Abstract. Like a call, but the compiler is expected to deal with it.
(defclass primop (instruction)
  ((%info :initarg :info :reader info
          :type cleavir-primop-info:info)))

;; primop returning values
(defclass vprimop (primop) ())

;; primop that tests in a branch
(defclass tprimop (primop no-output terminator) ())

(defclass abstract-call (one-output instruction)
  ((%attributes :initarg :attributes :reader attributes
                :initform (cleavir-attributes:default-attributes))
   (%transforms :initarg :transforms :reader transforms
                :initform nil)))
(defgeneric callee (instruction))

(defclass call (abstract-call) ())
(defmethod callee ((i call)) (first (inputs i)))

(defclass mv-call (abstract-call) ())
(defmethod callee ((i mv-call)) (first (inputs i)))

;;; A local call is a legal call to a function within the same
;;; module. Therefore, the first input is actually a
;;; FUNCTION. Importantly, illegal calls (i.e. ones whose arguments
;;; are not compatible with the lambda list of the callee) are not
;;; classified as local calls, both so that they can reuse the same
;;; runtime mechanism for argument count errors that normal calls from
;;; outside a module use, and so that we can make the assumption that
;;; local calls have compatible arguments with the lambda list of the
;;; callee.
(defclass abstract-local-call (abstract-call) ())

(defclass local-call (abstract-local-call) ())
(defmethod callee ((i local-call))
  (let ((function (first (inputs i))))
    (check-type function function)
    function))

(defclass mv-local-call (abstract-local-call) ())
(defmethod callee ((i mv-local-call)) (first (inputs i)))

(defclass returni (one-input no-output terminator0) ())

(defclass values-save
    (dynamic-environment one-input one-output terminator1)
  ())

(defclass values-collect (one-output instruction) ())

;;; Allocate some temporary space for an object of the specified rtype.
;;; Within this dynamic environment, readtemp and writetemp can be used.
;;; It is expected that the memory is freed whenever the dynamic environment
;;; is exited. This can be used to implement dynamic-extent or
;;; multiple-value-prog1.
;;; By "freed", I mean that (tagbody 0 (multiple-value-prog1 (f) (go 0))) and
;;; the like shouldn't eat the entire stack.
(defclass alloca (no-input no-output ssa dynamic-environment terminator1)
  ((%rtype :initarg :rtype :reader rtype)))

;;; Abstract.
(defclass accesstemp (instruction)
  (;; The storage this instruction is accessing.
   ;; Must be the instruction's dynamic environment,
   ;; or that environment's ancestor.
   (%alloca :initarg :alloca :reader alloca :type alloca)))

;;; Read the object stored in the temporary storage in the alloca.
(defclass readtemp (accesstemp no-input one-output instruction) ())

;;; Write it
(defclass writetemp (accesstemp one-input no-output instruction) ())

(defclass catch (no-input no-output lexical ssa dynamic-environment terminator)
  ((%unwinds :initarg :unwinds :accessor unwinds
             :initform (cleavir-set:empty-set)
             ;; A set of corresponding UNWINDs
             :type cleavir-set:set)))

;;; Mark a lexical binding.
(defclass leti (writevar) ())

;;; Mark an explicit dynamic-extent lexical binding, so that extent is
;;; explicitly represented.
(defclass dynamic-leti (leti terminator1 dynamic-environment)
  ())

;;; Nonlocal control transfer.
;;; Inputs are passed to the destination.
(defclass unwind (terminator0)
  ((%catch :initarg :catch :reader catch
           :type catch)
   (%destination :initarg :destination :reader destination
                 :type iblock)))

;;; Unconditional local control transfer. Inputs are passed to the single next
;;; block.
(defclass jump (terminator1) ())

;; Is the dynamic environment of the jump's iblock distinct from the
;; dynamic environment the jump is transferring control to?
(defmethod unwindp ((instruction jump))
  (not (eq (dynamic-environment (iblock instruction))
           (dynamic-environment (first (next instruction))))))

;;; IF instruction is a terminator which takes one input, tests it
;;; against NIL, and branches to either of its two successors. This is
;;; the canonical way to branch in Cleavir, which optimizations know
;;; how to deal with. Its sole input must be a CONDITIONAL-TEST.
(defclass ifi (one-input no-output terminator) ())

;;; A CONDITIONAL-TEST instruction is a computation whose value is
;;; guaranteed to be used by IFI as dispatch. The reason for this
;;; constraint is that these can usually be specially treated by a
;;; backend.
(defclass conditional-test (one-output instruction) ())

(defclass eq-test (conditional-test) ())
(defclass typeq-test (one-input conditional-test)
  ((%test-ctype :initarg :test-ctype :reader test-ctype)))

(defclass case (one-input no-output terminator)
  ((%comparees :initarg :comparees :reader comparees)))

;;; Convert an aggregate of :objects into a :multiple-values
(defclass fixed-to-multiple (one-output instruction) ())

;;; Reverse of the above
(defclass multiple-to-fixed (one-input instruction) ())

;;; Convert a value from one rtype to another.
;;; This may or may not entail an actual operation at runtime.
(defclass cast (one-input one-output instruction)
  (;; The destination rtype.
   ;; (The source rtype is the rtype of the input.)
   (%rtype :initarg :rtype :reader rtype)))

;;; Represents a type assertion on the first input.
(defclass thei (one-input one-output instruction)
  ((%asserted-type :initarg :asserted-type :accessor asserted-type)
   ;; This slot holds either a function which checks the input,
   ;; :TRUSTED if we want to treat this as trusted type assertion with
   ;; no check needed, or :EXTERNAL if a type check is needed but done
   ;; elsewhere.
   (%type-check-function :initarg :type-check-function
                         :accessor type-check-function
                         :type (or (member :trusted :external) function))))
