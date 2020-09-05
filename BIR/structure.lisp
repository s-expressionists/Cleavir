(in-package #:cleavir-bir)

;;; A "representation type", indicating an "underlying" type of an object
;;; in broad strokes.
(deftype rtype ()
  `(or (member :object ; a general lisp object
               :single-float ; an unboxed float
               :double-float ; "
               :continuation ; client dependent
               :multiple-values) ; a bit special
       (vector t))) ; an aggregate

(defun aggregatep (rtype)
  (typep rtype '(vector t)))

(defun aggregate (&rest rtypes)
  (apply #'vector rtypes))

(defun aggregate-length (aggregate) (length aggregate))

(defun aggregate-elt (aggregate n)
  (check-type aggregate (vector t))
  (aref aggregate n))

(defun make-aggregate (n rtype)
  (check-type n (integer 0))
  (make-array n :initial-element rtype))

(defun rtype= (rt1 rt2)
  (if (vectorp rt1)
      (and (vectorp rt2)
           (= (length rt1) (length rt2))
           (every #'rtype= rt1 rt2))
      (eq rt1 rt2)))

;;; Abstract. Something that can serve as a dynamic environment.
(defclass dynamic-environment () ())

(defclass datum ()
  ((%rtype :initarg :rtype :reader rtype
           :writer (setf %rtype)
           :type rtype)))

(defgeneric definitions (datum))
(defgeneric uses (datum))

;;; A datum with only one definition - itself.
(defclass value (datum) ())
(defmethod definitions ((datum value)) (make-set datum))

;;; A datum with only one use.
(defclass linear-datum (datum)
  ((%user :initarg :user :reader user :accessor %user
          :type instruction)))
(defmethod uses ((datum linear-datum)) (make-set (user datum)))

;;; A datum with one definition and one use.
(defclass transfer (value linear-datum) ())

;;; TODO: Using this uniformly will be work.
(defclass constant (transfer)
  ((%value :initarg :value :reader constant-value)))

;;; TODO: These are bad, but AST changes will be required to fix it.
(defclass immediate (transfer)
  ((%value :initarg :value :reader immediate-value)))
(defclass load-time-value (transfer)
  ((%form :initarg :form :reader form)
   (%read-only-p :initarg :read-only-p :reader read-only-p)))

(defclass instruction ()
  ((%predecessor :initarg :predecessor :accessor predecessor
                 :initform nil
                 ;; NIL indicates this is the first in a iblock.
                 :type (or instruction null))
   (%successor :initarg :successor :accessor successor
               ;; NIL indicates this is a terminator.
               :type (or instruction null))
   ;; A list of LINEAR-DATUM inputs to the instruction.
   ;; Note that an operation can use other data
   ;; e.g. a READVAR uses its VARIABLE.
   (%inputs :initarg :inputs :accessor inputs
            :type list)))

;;; An operation that outputs a value.
(defclass computation (transfer instruction) ())

;;; An operation that does not output a value.
(defclass operation (instruction) ())

(defclass no-input-mixin (instruction)
  ((%inputs :initform nil :type null)))

(defclass one-input-mixin (instruction)
  ((%inputs :type (cons value null))))

;;; An instruction that can end a iblock (abstract)
(defclass terminator (instruction)
  ((%successor :initform nil :type null)
   (%next :initarg :next :reader next
          ;; A list of iblocks.
          :type list)))

;;; A terminator with no next iblocks (abstract)
(defclass terminator0 (terminator operation)
  ((%next :initform nil :type null)))

;;; A terminator with exactly one next iblock (abstract)
(defclass terminator1 (terminator)
  ((%next :type (cons iblock null))))

;;; An argument to an iblock.
(defclass argument (linear-datum)
  ((%iblock :initarg :iblock :reader iblock
            :type iblock)))

;;; A modifiable lexical variable.
;;; Has to be read from and written to via instructions.
(defclass variable (datum)
  (;; Indicates the shared-ness of the variable.
   (%extent :initarg :extent :accessor extent
            :initform :unanalyzed
            :type (member :unanalyzed
                          :local ; only in one function.
                          ;;:dynamic ; TODO
                          :indefinite))
   ;; The "owner" of the variable is the function that
   ;; (a) accesses the variable, and
   ;; (b) encloses, directly or indirectly, all other functions that access
   ;;     the variable.
   ;; Until computed by analyze-variables, it's NIL.
   (%owner :initform nil :accessor owner
           :type (or null function))
   ;; Set of writevar instructions for this variable
   (%writers :initarg :writers :accessor writers :reader definitions
             :initform (empty-set)
             :type set)
   ;; " readvars
   (%readers :initarg :readers :accessor readers :reader uses
             :initform (empty-set)
             :type set)
   ;; " encloses (empty until closure conversion)
   (%encloses :initform (empty-set) :accessor encloses
              :type set)
   (%rtype :initarg :rtype :reader rtype
           :initform :object
           :type rtype)))

;;; TODO: This will implicate load form bla bla bla stuff.
(defun make-constant (value)
  (make-instance 'constant :value value))

;;; A sequence of instructions with no branching.
(defclass iblock ()
  ((%start :initarg :start :accessor start
           :type instruction)
   (%end :initarg :end :accessor end
         :type terminator)
   ;; NOTE: Should be a weak set
   (%predecessors :initarg :predecessors :accessor predecessors
                  :initform (empty-set)
                  ;; A set of blocks.
                  :type set)
   (%inputs :initarg :inputs :accessor inputs
            :initform nil
            ;; A list of ARGUMENTs
            :type list)
   ;; A list of IBLOCKs that enter this function nonlocally
   ;; (i.e. with an UNWIND operation).
   ;; NOTE: Should be a weak set
   (%entrances :initarg :entrances :accessor entrances
               :initform nil
               :type list)
   (%dynamic-environment :initarg :dynamic-environment
                         :accessor dynamic-environment
                         :type dynamic-environment)))

(defclass function (dynamic-environment)
  (;; NOTE: Should be a weak set
   (%iblocks :initarg :iblocks :reader iblocks :accessor %iblocks
            :initform (empty-set)
            :type set)
   (%start :initarg :start :accessor start
           :type iblock)
   ;; NOTE: Could be a weak reference
   ;; (for functions that never return)
   (%end :initarg :end :accessor end
         :type iblock)
   ;; FIXME: have multiple entry points instead
   (%lambda-list :initarg :lambda-list :reader lambda-list)
   ;; The set of variables accessed by this function.
   (%variables :initarg :variables :accessor variables
               :type set)))
