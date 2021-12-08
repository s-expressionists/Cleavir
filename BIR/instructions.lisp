(in-package #:cleavir-bir)

;;; This instruction creates a first-class object from a FUNCTION.
(defclass enclose (no-input one-output instruction)
  ((%code :initarg :code :reader code
          :type function)
   ;; Indicates the extent of the closure created by this
   ;; instruction.
   (%extent :initarg :extent :accessor extent
            :initform :indefinite
            :type (member :dynamic :indefinite))))

(defclass unreachable (no-input no-output terminator0) ())

(defclass nop (no-input no-output instruction) ())

;;; Abstract. An instruction dealing with a variable.
(defclass accessvar (instruction) ())

(defclass writevar (one-input one-output accessvar) ())

(defclass readvar (one-input one-output accessvar) ())

;;; Constants

(defclass constant-reference (one-input one-output instruction) ())

(defclass load-time-value-reference (one-input one-output instruction) ())

;;; Like a call, but the compiler is expected to deal with it.
;;; May or may not have outputs
(defclass primop (instruction)
  ((%info :initarg :info :reader info
          :type primop-info:info)))

(defmethod attributes ((instruction primop))
  (primop-info:attributes (info instruction)))

(defclass abstract-call (one-output instruction)
  ())

(defgeneric callee (instruction))

(defclass call (abstract-call) ())
(defmethod callee ((i call)) (first (inputs i)))

(defclass mv-call (abstract-call) ())
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

;;; Simpler version of values-save used when the number of values is
;;; known statically. Transformed from values-save by meta-evaluate.
(defclass fixed-values-save (values-save)
  ((%nvalues :initarg :nvalues :reader nvalues)))

(defclass values-collect (one-output instruction) ())

(defclass catch (no-input no-output lexical ssa dynamic-environment terminator)
  ((%unwinds :initarg :unwinds :accessor unwinds
             :initform (set:empty-set)
             ;; A set of corresponding UNWINDs
             :type set:set)))

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
;;; how to deal with.
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
