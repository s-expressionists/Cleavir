(in-package #:cleavir-bir)

(defclass enclose (no-input computation)
  ((%code :initarg :code :reader code
          :type function)
   ;; Indicates the extent of the closure created by this
   ;; instructions.
   (%extent :initarg :extent :accessor extent
            :initform :indefinite
            :type (member :dynamic :indefinite))))
(defmethod rtype ((d enclose)) :object)

(defclass unreachable (no-input no-output terminator0) ())

(defclass nop (no-input no-output operation) ())

;;; Abstract. An instruction dealing with a variable.
;;; It is assumed the variable is passed to make-instance rather
;;; than set later, and also that the instruction isn't reinitialized.
(defclass accessvar (instruction) ())

(defclass writevar (one-input accessvar operation) ())

(defmethod initialize-instance :after
    ((i writevar) &rest initargs &key outputs)
  (declare (cl:ignore initargs))
  (cleavir-set:nadjoinf (writers (first outputs)) i)
  i)

(defclass readvar (one-input accessvar computation) ())

(defmethod rtype ((rv readvar)) (rtype (first (inputs rv))))

(defmethod initialize-instance :after
    ((i readvar) &rest initargs &key inputs)
  (declare (cl:ignore initargs))
  (cleavir-set:nadjoinf (readers (first inputs)) i)
  i)

;;; Constants

(defclass constant-reference (one-input computation) ())

(defmethod rtype ((inst constant-reference)) :object)

(defmethod initialize-instance :after
    ((i constant-reference) &rest initargs &key inputs)
  (declare (cl:ignore initargs))
  (cleavir-set:nadjoinf (readers (first inputs)) i)
  i)

(defun make-constant-reference (constant)
  (make-instance 'constant-reference :inputs (list constant)))

;;; Load time value

;;; FIXME: This ought to be an instruction defined by the
;;; client, since load-time-value could be compiled down in a myriad
;;; of ways.
(defclass load-time-value (no-input computation)
  ((%form :initarg :form :reader form)
   (%read-only-p :initarg :read-only-p :reader read-only-p)))

(defmethod rtype ((inst load-time-value)) :object)

;;; Abstract. Like a call, but the compiler is expected to deal with it.
(defclass primop (instruction)
  ((%info :initarg :info :reader info
          :type primop-info)))

;; primop returning no values
(defclass nvprimop (primop no-output operation) ())

;; primop returning values
(defclass vprimop (primop computation) ())
(defmethod rtype ((d vprimop)) (first (out-rtypes (info d))))

;; primop that tests in a branch
(defclass tprimop (primop no-output terminator operation) ())

(defclass abstract-call (computation)
  ((%attributes :initarg :attributes :reader attributes
                :initform (cleavir-attributes:default-attributes))
   (%transforms :initarg :transforms :reader transforms
                :initform nil)))
(defgeneric callee (instruction))
(defmethod rtype ((d abstract-call)) :multiple-values)

(defclass call (abstract-call) ())
(defmethod callee ((i call)) (first (inputs i)))

(defclass mv-call (abstract-call) ())
(defmethod callee ((i mv-call)) (first (inputs i)))

;; A local call is a call to a function within the same
;; module. Therefore, the first input is actually a FUNCTION..
(defclass local-call (abstract-call) ())
(defmethod callee ((i local-call))
  (let ((function (first (inputs i))))
    (check-type function function)
    function))

(defclass returni (one-input no-output terminator0) ())

;;; Allocate some temporary space for an object of the specified rtype.
;;; Within this dynamic environment, readtemp and writetemp can be used.
;;; It is expected that the memory is freed whenever the dynamic environment
;;; is exited. This can be used to implement dynamic-extent or
;;; multiple-value-prog1.
;;; By "freed", I mean that (tagbody 0 (multiple-value-prog1 (f) (go 0))) and
;;; the like shouldn't eat the entire stack.
(defclass alloca (dynamic-environment no-input no-output terminator1 operation)
  ((%rtype :initarg :rtype :reader rtype)))

;;; Abstract.
(defclass accesstemp (instruction)
  (;; The storage this instruction is accessing.
   ;; Must be the instruction's dynamic environment,
   ;; or that environment's ancestor.
   (%alloca :initarg :alloca :reader alloca :type alloca)))

;;; Read the object stored in the temporary storage in the alloca.
(defclass readtemp (accesstemp no-input computation) ())
(defmethod rtype ((d readtemp)) (rtype (alloca d)))

;;; Write it
(defclass writetemp (accesstemp one-input no-output operation) ())

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
(defclass jump (terminator1 operation) ())

;; Is the dynamic environment of the jump's iblock distinct from the
;; dynamic environment the jump is transferring control to?
(defmethod unwindp ((instruction jump))
  (not (eq (dynamic-environment (iblock instruction))
           (dynamic-environment (first (next instruction))))))

;;; EQ
(defclass eqi (no-output terminator operation) ())

;;; FIXME: Should take a ctype rather than a type specifier.
(defclass typeq (one-input no-output terminator operation)
  ((%type-specifier :initarg :type-specifier :reader type-specifier)))

(defclass typew (one-input no-output terminator operation)
  ((%ctype :initarg :ctype :reader ctype)))

;;; Used to indicate to type inference not to continue along a path.
(defclass choke (no-input no-output terminator1 operation) ())

(defclass case (one-input no-output terminator operation)
  ((%comparees :initarg :comparees :reader comparees)))

;;; Convert an aggregate of :objects into a :multiple-values
(defclass fixed-to-multiple (computation) ())
(defmethod rtype ((d fixed-to-multiple)) :multiple-values)

;;; Reverse of the above
(defclass multiple-to-fixed (one-input operation) ())

;;; Convert a value from one rtype to another.
;;; This may or may not entail an actual operation at runtime.
(defclass cast (one-input computation)
  (;; The destination rtype.
   ;; (The source rtype is the rtype of the input.)
   (%rtype :initarg :rtype :reader rtype)))
