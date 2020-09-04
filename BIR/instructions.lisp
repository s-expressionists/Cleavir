(in-package #:cleavir-bir)

(defclass initialize-closure (operation) ())

(defclass enclose (no-input-mixin computation)
  ((%code :initarg :code :reader code
          :type function)
   (%initializer :initarg :initializer :reader initializer
                 :type initialize-closure)
   (%rtype :initform :object)))

(defclass unreachable (no-input-mixin terminator0) ())

(defclass nop (no-input-mixin operation) ())

;;; Abstract. An instruction dealing with a variable.
;;; It is assumed the variable is passed to make-instance rather
;;; than set later, and also that the instruction isn't reinitialized.
(defclass accessvar (instruction)
  ((%variable :initarg :variable :reader variable :type variable)))

(defclass writevar (one-input-mixin accessvar operation) ())

(defclass readvar (no-input-mixin accessvar computation)
  ((%rtype :initform :object)))

;;; Abstract. Like a call, but the compiler is expected to deal with it.
(defclass primop (instruction)
  ((%primop-info :initarg :primop-info :reader primop-info
                 :type primop-info)))

;; primop returning no values
(defclass nvprimop (primop operation) ())

;; primop returning values
(defclass vprimop (primop computation) ())

(defmethod initialize-instance :after ((p vprimop) &rest initargs)
  (declare (ignore initargs))
  (setf (%rtype p) (rtype (primop-info p))))

(defclass call (computation)
  ((%rtype :initform :multiple-values :type (eql :multiple-values))))

(defclass returni (one-input-mixin terminator0) ())

;;; Allocate some temporary space for an object of the specified rtype.
;;; Within this dynamic environment, readmem and writemem can be used.
;;; It is expected that the memory is freed whenever the dynamic environment
;;; is exited. This can be used to implement dynamic-extent or
;;; multiple-value-prog1.
;;; By "freed", I mean that (tagbody 0 (multiple-value-prog1 (f) (go 0))) and
;;; the like shouldn't eat the entire stack.
;;; FIXME: Make this a computation and have readtemp and writetemp refer to it

;;; explicitly rather than implicitly thorugh the dynenv.
(defclass alloca (dynamic-environment no-input-mixin terminator1 operation)
  ())

;;; Abstract. NOTE: Might have to specify which dynamic environment to access?
(defclass accesstemp (instruction) ())

;;; Read the object stored in the temporary storage in the dynamic env.
(defclass readtemp (accesstemp no-input-mixin computation) ())

;;; Write it
(defclass writetemp (accesstemp one-input-mixin operation) ())

(defclass catch (dynamic-environment no-input-mixin terminator computation)
  (;; NOTE: Should be a weak set
   (%unwinds :initarg :unwinds :accessor unwinds
             :initform (empty-set)
             ;; A set of corresponding UNWINDs
             :type set)
   (%rtype :initform :continuation :type (eql :continuation))))

;;; Nonlocal control transfer.
;;; First input is the continuation.
;;; Remaining inputs are passed to the destination.
(defclass unwind (terminator0)
  ((%catch :initarg :catch :reader catch
           :type catch)
   (%destination :initarg :destination :reader destination
                 :type iblock)))

;;; Local control transfer. Inputs are passed to the next block.
;;; This is essentially just jump, but also indicates that the next block may
;;; have a distinct dynamic environment.
(defclass local-unwind (terminator1 operation) ())

;;; Go to the next iblock, passing the inputs as arguments.
(defclass jump (terminator1 operation) ())

;;; EQ
(defclass eqi (terminator operation) ())

;;; Convert an aggregate of :objects into a :multiple-values
(defclass fixed-to-multiple (one-input-mixin computation)
  ((%rtype :initform :multiple-values :type (eql :multiple-values))))

;;; Reverse of the above
(defclass multiple-to-fixed (one-input-mixin computation) ())

;;; Extract a value from an aggregate value.
(defclass extract (one-input-mixin computation)
  (;; Index of the field to extract.
   (%index :initarg :index :reader index
           :type (integer 0))))

(defmethod (setf inputs) :before (nv (inst extract))
  (assert (= (length nv) 1))
  (let ((in-rt (rtype (first nv)))
        (i (index inst)))
    (assert (and (aggregatep in-rt)
                 (> (aggregate-length in-rt) i)))))

(defmethod (setf inputs) :after (nv (inst extract))
  (setf (%rtype inst)
        (aggregate-elt (rtype (first nv)) (index inst))))

;;; Create an aggregate value from a sequence of values.
(defclass create (computation) ())

(defmethod (setf inputs) :after (nv (inst create))
  (setf (%rtype inst) (apply #'aggregate (mapcar #'rtype nv))))

;;; Convert a value from one type to another.
;;; This may or may not entail an actual operation at runtime.
(defclass cast (one-input-mixin computation) ())
