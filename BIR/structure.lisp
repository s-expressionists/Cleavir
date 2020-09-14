(in-package #:cleavir-bir)

;;; A "representation type", indicating an "underlying" type of an object
;;; in broad strokes.
(deftype rtype ()
  `(member :object ; a general lisp object
           :single-float ; an unboxed float
           :double-float ; "
           :continuation ; client dependent
           :multiple-values)) ; a bit special

(defun rtype= (rt1 rt2) (eq rt1 rt2))

;;; Abstract. Something that can serve as a dynamic environment.
(defclass dynamic-environment () ())
(defun parent (dynamic-environment)
  (if (typep dynamic-environment 'function)
      nil
      (dynamic-environment (iblock dynamic-environment))))

;;; Abstract. Something that can bind lexical variables.
(defclass lexical-bind (dynamic-environment) ())
;;; Set of variables it binds.
(defgeneric bindings (lexical-bind))

(defclass datum ()
  ((%rtype :initarg :rtype :reader rtype
           :writer (setf %rtype)
           :type rtype)))

(defgeneric definitions (datum))
(defgeneric uses (datum))

;;; A datum with only one definition (static single assignment).
(defclass ssa (datum) ())
(defgeneric definition (ssa))
(defmethod definitions ((datum ssa)) (cleavir-set:make-set datum))

;;; A datum with only one use.
(defclass linear-datum (datum)
  ((%use :initarg :use :reader use :accessor %use
         :type instruction)))
(defmethod uses ((datum linear-datum)) (cleavir-set:make-set (use datum)))

;;; A datum with one definition and one use.
(defclass transfer (ssa linear-datum) ())

;;; An SSA datum with only one definition - itself.
(defclass value (ssa) ())
(defmethod definition ((datum value)) datum)

;;; TODO: Using this uniformly will be work.
(defclass constant (value transfer)
  ((%value :initarg :value :reader constant-value)))

;;; TODO: These are bad, but AST changes will be required to fix it.
(defclass immediate (value transfer)
  ((%value :initarg :value :reader immediate-value)))
(defclass load-time-value (value transfer)
  ((%form :initarg :form :reader form)
   (%read-only-p :initarg :read-only-p :reader read-only-p)
   (%rtype :initform :object)))

;;; An instruction is something to be done.
;;; All instructions have sequences of inputs and outputs.
;;; Inputs are mutable but outputs are not.
;;; Every input and output is a LINEAR-DATUM, except that READVAR has a VARIABLE
;;; input and WRITEVAR has one as an output.
(defgeneric inputs (instruction))
(defgeneric (setf inputs) (new-inputs instruction))
(defgeneric outputs (instruction))
(defclass instruction ()
  ((%predecessor :initarg :predecessor :accessor predecessor
                 :initform nil
                 ;; NIL indicates this is the first in a iblock.
                 :type (or instruction null))
   (%successor :initarg :successor :accessor successor
               ;; NIL indicates this is a terminator.
               :type (or instruction null))
   (%inputs :initarg :inputs :accessor inputs
            ;; Sequence of DATA.
            :type sequence)
   ;; The iblock this instruction belongs to.
   (%iblock :initarg :iblock :accessor iblock :type iblock)))

;;; Shortcuts to get an instruction's owner
(defmethod function ((instruction instruction))
  (function (iblock instruction)))
;;; and dynamic environment.
(defmethod dynamic-environment ((instruction instruction))
  (dynamic-environment (iblock instruction)))

;;; An instruction that outputs a single datum.
;;; In this case the instruction is identified with the datum.
(defclass computation (value transfer instruction) ())
(defmethod outputs ((instruction computation)) (list instruction))

;;; An instruction that outputs a variable number of outputs
;;; or a fixed number (that is not one) of them.
(defclass operation (instruction)
  (;; Sequence of data.
   (%outputs :initarg :outputs :reader outputs :accessor %outputs
             :type sequence)))

;;; Data output by an OPERATION.
;;; (If a terminator, PHIs are output instead.)
(defclass output (transfer)
  ((%definition :initarg :definition :reader definition)))

;;; some useful mixins
(defclass no-input (instruction)
  ((%inputs :initform nil :type null)))
(defclass one-input (instruction)
  ((%inputs :type (cons value null))))
(defclass no-output (operation)
  ((%outputs :initform nil :type null)))

;;; An instruction that can end a iblock (abstract)
(defclass terminator (instruction)
  ((%successor :initform nil :type null)
   (%next :initarg :next :accessor next
          ;; A list of iblocks.
          :type list)))

;;; A terminator with no next iblocks (abstract)
(defclass terminator0 (terminator operation)
  ((%next :initform nil :type null)))

;;; A terminator with exactly one next iblock (abstract)
(defclass terminator1 (terminator)
  ((%next :type (cons iblock null))))

;;; An argument to a function.
(defclass argument (value transfer) ())

;;; An argument to an iblock.
(defclass phi (linear-datum)
  ((%iblock :initarg :iblock :reader iblock
            :type iblock)))
(defmethod definitions ((phi phi))
  (let ((ib (iblock phi)))
    (cleavir-set:nunion
     (cleavir-set:mapset 'cleavir-set:set #'end (predecessors ib))
     (cleavir-set:mapset 'cleavir-set:set #'end (entrances ib)))))

;;; A mutable lexical variable.
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
   ;; The LEXICAL-BIND that binds this.
   (%binder :initarg :binder :accessor binder :type lexical-bind)
   (%definitions :initarg :definitions :reader definitions
                 :accessor writers
                 :initform (cleavir-set:empty-set)
                 ;; All WRITEVAR instructions.
                 :type cleavir-set:set)
   (%uses :initarg :uses :accessor readers :reader uses
          :initform (cleavir-set:empty-set)
          ;; All READVAR instructions.
          :type cleavir-set:set)
   ;; Set of encloses (empty until closure conversion)
   ;; These are not exactly definitions or uses, since the function
   ;; being enclosed can do both and conceptually needs the variable
   ;; itself rather than its value. Thus the slot.
   (%encloses :initform (cleavir-set:empty-set) :accessor encloses
              :type cleavir-set:set)
   (%rtype :initform :object)))

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
                  :initform (cleavir-set:empty-set)
                  ;; A set of blocks.
                  :type cleavir-set:set)
   (%inputs :initarg :inputs :accessor inputs
            :initform nil
            ;; A sequence of PHIs
            :type sequence)
   ;; A set of IBLOCKs that enter this function nonlocally
   ;; (i.e. with an UNWIND operation).
   ;; NOTE: Should be a weak set
   (%entrances :initarg :entrances :accessor entrances
               :initform (cleavir-set:empty-set)
               :type cleavir-set:set)
   (%dynamic-environment :initarg :dynamic-environment
                         :accessor dynamic-environment
                         :type dynamic-environment)
   ;; The function this belongs to.
   (%function :initarg :function :reader function :type function)))

(defun successors (iblock)
  (next (end iblock)))

(defclass function (lexical-bind)
  (;; NOTE: Should be a weak set
   (%iblocks :initarg :iblocks :reader iblocks :accessor %iblocks
            :initform (cleavir-set:empty-set)
            :type cleavir-set:set)
   (%start :initarg :start :accessor start
           :type iblock)
   ;; NOTE: Could be a weak reference
   ;; (for functions that never return)
   (%end :initarg :end :accessor end
         :type iblock)
   ;; FIXME: have multiple entry points instead
   (%lambda-list :initarg :lambda-list :accessor lambda-list)
   ;; The set of variables accessed by this function.
   (%variables :initarg :variables :accessor variables
               :type cleavir-set:set)
   ;; The set of ENCLOSE instructions with this as their CODE.
   (%encloses :initform (cleavir-set:empty-set) :accessor encloses
              :type cleavir-set:set)))

(defmethod bindings ((function function))
  (cleavir-set:filter 'cleavir-set:set
                      (lambda (v) (eq (owner v) function))
                      (variables function)))

;;; The set of blocks in a function that have nonlocal entrances.
(defmethod entrances ((function function))
  (cleavir-set:filter
   'cleavir-set:set
   (lambda (ib) (not (cleavir-set:empty-set-p (entrances ib))))
   (iblocks function)))

;;; The set of blocks in a function that nonlocally exit, i.e. are terminated
;;; by UNWIND instructions.
(defmethod exits ((function function))
  (cleavir-set:filter
   'cleavir-set:set
   (lambda (ib) (typep (end ib) 'unwind))
   (iblocks function)))
