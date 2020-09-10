(in-package #:cleavir-bir)

(defclass enclose (no-input computation)
  ((%code :initarg :code :reader code
          :type function)
   ;; The set of variables enclosed
   (%variables :accessor variables :initform (cleavir-set:empty-set)
               :type cleavir-set:set)
   (%rtype :initform :object)))

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

(defclass readvar (accessvar computation)
  ((%rtype :initform :object)))

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

(defmethod initialize-instance :after ((p vprimop) &rest initargs)
  (declare (ignore initargs))
  (setf (%rtype p) (rtype (info p))))

(defclass call (computation)
  ((%rtype :initform :multiple-values :type (eql :multiple-values))))

(defclass returni (one-input no-output terminator0) ())

;;; Allocate some temporary space for an object of the specified rtype.
;;; Within this dynamic environment, readmem and writemem can be used.
;;; It is expected that the memory is freed whenever the dynamic environment
;;; is exited. This can be used to implement dynamic-extent or
;;; multiple-value-prog1.
;;; By "freed", I mean that (tagbody 0 (multiple-value-prog1 (f) (go 0))) and
;;; the like shouldn't eat the entire stack.
;;; FIXME: Make this a computation and have readtemp and writetemp refer to it

;;; explicitly rather than implicitly thorugh the dynenv.
(defclass alloca (dynamic-environment no-input no-output terminator1 operation)
  ())

;;; Abstract. NOTE: Might have to specify which dynamic environment to access?
(defclass accesstemp (instruction) ())

;;; Read the object stored in the temporary storage in the dynamic env.
(defclass readtemp (accesstemp no-input computation) ())

;;; Write it
(defclass writetemp (accesstemp one-input no-output operation) ())

(defclass catch (no-input lexical-bind terminator computation)
  (;; NOTE: Should be a weak set
   (%unwinds :initarg :unwinds :accessor unwinds
             :initform (cleavir-set:empty-set)
             ;; A set of corresponding UNWINDs
             :type cleavir-set:set)
   (%rtype :initform :continuation :type (eql :continuation))))
(defmethod bindings ((lb catch))
  (check-type (use catch) writevar)
  (cleavir-set:make-set (first (outputs (use catch)))))

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

(defclass case (one-input no-output terminator operation)
  ((%comparees :initarg :comparees :reader comparees)))

;;; Convert an aggregate of :objects into a :multiple-values
(defclass fixed-to-multiple (computation)
  ((%rtype :initform :multiple-values :type (eql :multiple-values))))

;;; Reverse of the above
(defclass multiple-to-fixed (one-input operation) ())

;;; Given a linear-datum and a list of rtypes, return two values:
;;; A new multiple-to-fixed, and and a sequence of its outputs.
;;; This is necessary because definers and outputs are both immutable.
(defun make-multiple-to-fixed (input rtypes)
  (let* ((mtf (make-instance 'multiple-to-fixed :inputs (list input)))
         (outputs
           (mapcar (lambda (rt) (make-instance 'output
                                  :definition mtf :rtype rt))
                   rtypes)))
    (setf (%outputs mtf) outputs)
    (values mtf outputs)))

;;; Convert a value from one type to another.
;;; This may or may not entail an actual operation at runtime.
(defclass cast (one-input computation) ())
