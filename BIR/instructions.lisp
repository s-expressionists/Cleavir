(in-package #:cleavir-bir)

(defclass enclose (no-input computation)
  ((%code :initarg :code :reader code
          :type function)
   ;; The set of variables enclosed
   (%variables :accessor variables :initform (cleavir-set:empty-set)
               :type cleavir-set:set)))
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
  (declare (ignore initargs))
  (cleavir-set:nadjoinf (writers (first outputs)) i)
  i)

(defclass readvar (one-input accessvar computation) ())

(defmethod rtype ((rv readvar)) (rtype (first (inputs rv))))

(defmethod initialize-instance :after
    ((i readvar) &rest initargs &key inputs)
  (declare (ignore initargs))
  (cleavir-set:nadjoinf (readers (first inputs)) i)
  i)

;;; Abstract. Like a call, but the compiler is expected to deal with it.
(defclass primop (instruction)
  ((%info :initarg :info :reader info
          :type primop-info)))

;; primop returning no values
(defclass nvprimop (primop no-output operation) ())

;; primop returning values
(defclass vprimop (primop computation) ())
(defmethod rtype ((d vprimop)) (first (out-rtypes (info d))))

(defclass abstract-call (computation)
  ((%attributes :initarg :attributes :reader attributes
                :initform (cleavir-attributes:default-attributes))))
(defgeneric callee (instruction))
(defmethod rtype ((d abstract-call)) :multiple-values)

(defclass call (abstract-call) ())
(defmethod callee ((i call)) (first (inputs call)))

(defclass mv-call (abstract-call) ())
(defmethod callee ((i mv-call)) (first (inputs call)))

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

;;; Abstract. NOTE: Might have to specify which dynamic environment to access?
(defclass accesstemp (instruction) ())

;;; Read the object stored in the temporary storage in the dynamic env.
(defclass readtemp (accesstemp no-input computation) ())
(defmethod rtype ((d readtemp)) (rtype (dynamic-environment d)))

;;; Write it
(defclass writetemp (accesstemp one-input no-output operation) ())

(defclass catch (no-input lexical-bind terminator operation)
  (;; NOTE: Should be a weak set
   (%unwinds :initarg :unwinds :accessor unwinds
             :initform (cleavir-set:empty-set)
             ;; A set of corresponding UNWINDs
             :type cleavir-set:set)))
(defmethod rtype ((d catch)) :continuation)
(defmethod bindings ((catch catch))
  (cleavir-set:make-set (first (outputs catch))))

;;; Mark a lexical binding, so that cell extent is obvious.
(defclass leti (no-input no-output terminator1 lexical-bind operation)
  ((%bindings :initarg :bindings :accessor bindings
              :initform (cleavir-set:empty-set)
              :type cleavir-set:set)))

;;; Nonlocal control transfer.
;;; First input is the continuation.
;;; Remaining inputs are passed to the destination.
(defclass unwind (terminator0)
  ((%catch :initarg :catch :reader catch
           :type catch)
   (%destination :initarg :destination :reader destination
                 :type iblock)))

;;; Unconditional local control transfer. Inputs are passed to the single next
;;; block.
(defclass jump (terminator1 operation)
  (;; T if the dynamic environment of the next iblock is distinct from (a
   ;; parent of) the jump's iblock's.
   (%unwindp :initarg :unwindp :initform nil :reader unwindp :type boolean)))

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
