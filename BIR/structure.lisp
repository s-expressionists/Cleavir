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

;;; A value.
;;; May be a single value, an unknown number of values,
;;; or a fixed number of values.
(defclass value ()
  ((%rtype :initarg :rtype :reader rtype
           :writer (setf %rtype)
           :type rtype)))

;;; An argument to a function or iblock.
(defclass argument (value) ())

;;; A modifiable lexical variable.
;;; Has to be read from and written to via instructions.
(defclass variable ()
  ((%rtype :initarg :rtype :reader rtype
           :initform :object
           :type rtype)))

(defclass constant (value)
  ((%value :initarg :value :reader constant-value)))

;;; TODO: This will implicate load form bla bla bla stuff.
(defun make-constant (value)
  (make-instance 'constant :value value))

;;; An abstract operation
(defclass instruction ()
  ((%predecessor :initarg :predecessor :accessor predecessor
                 :initform nil
                 ;; NIL indicates this is the first in a iblock.
                 :type (or instruction null))
   (%successor :initarg :successor :accessor successor
               ;; NIL indicates this is a terminator.
               :type (or instruction null))
   (%inputs :initarg :inputs :accessor inputs
            ;; A list of VALUEs
            :type list)))

;;; An instruction that has no output
(defclass operation (instruction) ())

;;; An instruction that outputs
(defclass computation (instruction value) ())

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
   (%iblocks :initarg :iblocks :accessor iblocks
            :initform (empty-set)
            :type set)
   (%start :initarg :start :accessor start
           :type iblock)
   ;; NOTE: Could be a weak reference
   ;; (for functions that never return)
   (%end :initarg :end :accessor end
         :type iblock)
   (%inputs :initarg :inputs :reader inputs
            ;; A list of ARGUMENTs
            :type list)))
