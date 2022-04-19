(in-package #:cleavir-bir)

;;; Abstract. Something that can serve as a dynamic environment.
(defclass dynamic-environment ()
  (;; The set of iblocks that have this as their dynamic environment.
   (%scope :initarg :scope :accessor scope :initform (set:empty-set)
           :type set:set)))
(defun parent (dynamic-environment)
  (if (typep dynamic-environment 'function)
      nil
      (dynamic-environment (iblock dynamic-environment))))

(defgeneric origin (bir))

(defclass datum ()
  (;; A name, for debugging/display/etc. NIL means no name.
   (%name :initarg :name :initform nil :reader name
          :type (or symbol (cons symbol (cons symbol null)) ; e.g. (LABELS REC)
                    null)))
  (:metaclass stealth-mixins:victim-class))

;;; A lexical is a datum that can be bound in an environment.
(defclass lexical (datum) ()
  (:metaclass stealth-mixins:victim-class))

(defmethod print-object ((o datum) stream)
  (print-unreadable-object (o stream :type t :identity t)
    ;; NAME is always bound normally, but this safety is useful when debugging.
    (when (slot-boundp o '%name)
      (let ((name (name o)))
        (when name (write name :stream stream))))))

(defgeneric unused-p (datum))
(defgeneric ssa-p (datum))

;;; A datum with only one definition (static single assignment).
(defclass ssa (datum) ()
  (:metaclass stealth-mixins:victim-class))
(defmethod ssa-p ((ssa datum)) t)

;;; This type represents the type of the datum that we can assume when
;;; making inferences.
(defgeneric ctype (linear-datum))
;;; This type represents the type of the datum the code declares.
(defgeneric asserted-type (linear-datum))

;;; Default for ctypes.
(defvar *top-ctype*)
(defun current-top-ctype ()
  (if (boundp '*top-ctype*)
      *top-ctype*
      (ctype:values-top nil)))

;;; A datum with only one use.
(defclass linear-datum (datum)
  ((%use :initarg :use :initform nil :reader use :accessor %use
         :type (or null instruction))
   ;; A type that the code declares holds for this linear-datum.
   (%asserted-type :initform (current-top-ctype)
                   :initarg :asserted-type
                   :accessor asserted-type)
   ;; A type the compiler has proven holds for this linear-datum.
   (%derived-type :initform (current-top-ctype)
                  :initarg :derived-type
                  :writer (setf derived-type)
                  ;; For a generic linear datum, the type we use to
                  ;; make inferences is just the type the compiler has
                  ;; proven about this datum.
                  :reader ctype)
   ;; Additional flow attributes
   (%attributes :initarg :attributes :accessor attributes
                :initform (attributes:default-attributes)))
  (:metaclass stealth-mixins:victim-class))
(defmethod unused-p ((datum linear-datum))
  (null (use datum)))

;;; A datum with one definition and one use.
(defclass transfer (ssa linear-datum) ()
  (:metaclass stealth-mixins:victim-class))

;;; An SSA datum with only one definition - itself.
(defclass value (ssa) ()
  (:metaclass stealth-mixins:victim-class))

(defclass constant (value)
  ((%value :initarg :value :reader constant-value)
   (%readers :initform (set:empty-set) :accessor readers))
  (:metaclass stealth-mixins:victim-class))

(defmethod print-object ((object constant) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (constant-value object) :stream stream)))

(defclass load-time-value (value)
  ((%form :initarg :form :reader form)
   (%read-only-p :initarg :read-only-p :reader read-only-p)
   (%readers :initform (set:empty-set) :accessor readers))
  (:metaclass stealth-mixins:victim-class))

;;; These variables are used for defaulting the origin and policy.
;;; If they are not bound it should still be possible to make instructions,
;;; however; default values are NIL. (TODO: Is that right for policy?)
(defvar *policy*)
(defvar *origin*)
(defun current-policy () (if (boundp '*policy*) *policy* nil))
(defun current-origin () (if (boundp '*origin*) *origin* nil))

;;; An instruction is something to be done.
;;; All instructions have sequences of inputs and outputs.
;;; Every input and output is a LINEAR-DATUM, except that READVAR has a VARIABLE
;;; input and WRITEVAR has one as an output.
(defgeneric inputs (instruction))
(defgeneric (setf inputs) (new-inputs instruction))
(defgeneric outputs (instruction))
(defgeneric (setf outputs) (new-outputs instruction))

(defclass instruction ()
  ((%predecessor :initarg :predecessor :accessor predecessor
                 :initform nil
                 ;; NIL indicates this is the first in a iblock.
                 :type (or instruction null))
   (%successor :initarg :successor :accessor successor
               ;; NIL indicates this is a terminator.
               ;; FIXME: This type declaration causes crashes-to-LDB in SBCL
               ;; under unclear circumstances.
               :type #-sbcl (or instruction null)
                     #+sbcl (or standard-object null))
   (%inputs :initform nil :initarg :inputs :accessor inputs
            ;; Sequence of DATA.
            :type sequence);; Sequence of data.
   (%outputs :initform '() :initarg :outputs :accessor outputs
             :type sequence)
   ;; The iblock this instruction belongs to.
   (%iblock :initarg :iblock :accessor iblock :type iblock)
   (%policy :initform (current-policy) :initarg :policy :reader policy)
   (%origin :initform (current-origin) :initarg :origin :reader origin)))

;;; Shortcuts to get an instruction's owner
(defmethod function ((instruction instruction))
  (function (iblock instruction)))
;;; and dynamic environment.
(defmethod dynamic-environment ((instruction instruction))
  (dynamic-environment (iblock instruction)))

(defgeneric definitions (datum))

;;; Data output by an instruction.
;;; (If a terminator, PHIs are output instead.)
(defclass output (transfer)
  ((%definition :initform nil :initarg :definition
                :reader definition :accessor %definition))
  (:metaclass stealth-mixins:victim-class))

(defmethod definitions ((datum output))
  (set:make-set (definition datum)))

(defmethod origin ((datum output))
  (when (definition datum) (origin (definition datum))))

;;; some useful mixins
(defclass no-input (instruction)
  ((%inputs :initform nil :type null)))
(defclass one-input (instruction)
  ((%inputs :initform nil
            :type (or null (cons datum null)))))
(defclass no-output (instruction)
  ((%outputs :initform '() :type null)))
(defclass one-output (instruction)
  ((%outputs :initform nil
             :type (or null (cons datum null)))))

(defmethod input ((inst one-input)) (first (inputs inst)))
(defmethod output ((inst one-output)) (first (outputs inst)))

;;; An instruction that can end a iblock (abstract)
(defclass terminator (instruction)
  ((%successor :initform nil :type null)
   (%next :initarg :next :accessor next
          ;; A list of iblocks.
          :type list)))

;;; A terminator with no next iblocks (abstract)
(defclass terminator0 (terminator)
  ((%next :initform nil :type null)))

;;; A terminator with exactly one next iblock (abstract)
(defclass terminator1 (terminator)
  ((%next :type (or null (cons iblock null))))) ; can be null after metaevaluate

;;; An argument to a function.
(defclass argument (value transfer)
  ((%function :initarg :function :reader function :type function))
  (:metaclass stealth-mixins:victim-class))

;;; An ARGUMENT is unused if either it itself has no use or it's use
;;; is a LETI with no readers.
(defmethod unused-p ((datum argument))
  (or (call-next-method)
      (let ((use (use datum)))
        (and (typep use 'leti) (unused-p (output use))))))

;;; An argument to an iblock.
(defclass phi (linear-datum)
  ((%iblock :initarg :iblock :reader iblock
            :type iblock))
  (:metaclass stealth-mixins:victim-class))

(defmethod definitions ((phi phi))
  (let ((ib (iblock phi))
        (definitions (set:empty-set)))
    (set:doset (predecessor (predecessors ib))
      (let ((end (end predecessor)))
        (unless (typep end 'catch)
          (set:nadjoinf definitions end))))
    (set:doset (entrance (entrances ib))
      (set:nadjoinf definitions (end entrance)))
    definitions))

;;; The ``transitive'' use of a linear datum walks through jump/phi usages.
(defun transitive-use (linear-datum)
  (loop
    (let ((use (use linear-datum)))
      (unless (typep use 'jump)
        (return use))
      (setq linear-datum
            (nth (position linear-datum (inputs use))
                 (outputs use))))))

;;; Get the set of data that feed into the phi.
(defun phi-inputs (phi)
  (let* ((ib (iblock phi))
         (pos (position phi (inputs ib)))
         (inputs (set:empty-set)))
    (assert (not (null pos)))
    (set:doset (predecessor (predecessors ib))
      (let ((end (end predecessor)))
        (unless (typep end 'catch)
          (let ((in (nth pos (inputs end))))
            (assert (not (null in)))
            (set:nadjoinf inputs in)))))
    (set:doset (entrance (entrances ib))
      (let* ((end (end entrance))
             (in (nth pos (inputs end))))
        (assert (not (null in)))
        (set:nadjoinf inputs in)))
    inputs))

;;; A mutable lexical variable which must be read from and written to
;;; via READVAR and WRITEVAR instructions.
(defclass variable (lexical)
  (;; Indicates the extent of a lexical variable. Filled in by
   ;; variable extent analysis. Note that the dynamic extent of
   ;; variables themselves are induced by the extent of the closures
   ;; closing over it and independent of dynamic extent
   ;; declarations. A dynamic extent declaration on a variable is
   ;; instead supposed to mark the extent of the value it is bound
   ;; to. Therefore we carry that kind of dynamic extent on the binder
   ;; of the variable and propagate that back to implement the
   ;; semantics of dynamic extent declarations.
   (%extent :initarg :extent :accessor extent
            :initform :unanalyzed
            :type (member :unanalyzed
                          :local
                          :dynamic
                          :indefinite))
   ;; The LETI that binds this variable.
   (%binder :initarg :binder :accessor binder :type leti)
   (%writers :accessor writers
             :initform (set:empty-set)
             ;; All WRITEVAR instructions.
             :type set:set)
   (%readers :accessor readers
             :initform (set:empty-set)
             ;; All READVAR instructions.
             :type set:set)
   ;; Has this variable ever been used?
   (%use-status :initarg :use-status :initform nil :reader use-status
                :type (member nil set read))
   ;; What kind of ignore declaration is on this variable?
   (%ignore :initarg :ignore :reader ignore))
  (:metaclass stealth-mixins:victim-class))

(defmethod origin ((datum variable)) (origin (binder datum)))

(defmethod unused-p ((datum variable))
  (set:empty-set-p (readers datum)))

(defun record-variable-set (variable)
  (with-slots (%use-status) variable
    (or %use-status (setf %use-status 'set))))

(defun record-variable-ref (variable)
  (with-slots (%use-status) variable
    (setf %use-status 'read)))

(defmethod function ((v variable))
  (function (binder v)))

(defun immutablep (variable)
  (= (set:size (writers variable)) 1))

(defun closed-over-p (variable)
  (let ((owner (function variable)))
    (set:doset (reader (readers variable))
      (unless (eq owner (function reader))
        (return-from closed-over-p t)))
    (set:doset (writer (writers variable))
      (unless (eq owner (function writer))
        (return-from closed-over-p t)))))

;;; A sequence of instructions with no branching.
(defclass iblock ()
  ((%start :initarg :start :accessor start
           :type instruction)
   (%end :initarg :end :accessor end
         :type terminator)
   (%predecessors :initarg :predecessors :accessor predecessors
                  :initform (set:empty-set)
                  ;; A set of blocks.
                  :type set:set)
   (%inputs :initarg :inputs :accessor inputs
            :initform nil
            ;; A sequence of PHIs
            :type sequence)
   ;; A set of IBLOCKs that enter this function nonlocally
   ;; (i.e. with an UNWIND operation).
   (%entrances :initarg :entrances :accessor entrances
               :initform (set:empty-set)
               :type set:set)
   ;; The links for the doubly linked list of iblocks maintained in
   ;; forward flow order.
   (%next :initform nil :accessor %next :type (or null iblock))
   (%prev :initform nil :accessor %prev :type (or null iblock))
   ;; Slot used for flow order computation.
   (%reachedp :initform nil :accessor reachedp)
   (%dynamic-environment :initarg :dynamic-environment
                         :accessor dynamic-environment
                         :type dynamic-environment)
   ;; The function this belongs to.
   (%function :initarg :function :accessor function :type (or null function)) ; null is for deleted blocks
   ;; For debug/introspection
   (%name :initarg :name :reader name :initform nil)))

(defmethod print-object ((o iblock) s)
  (print-unreadable-object (o s :type t)
    (write (name o) :stream s)))

(defun iblock-started-p (iblock)
  (slot-boundp iblock '%start))

(defun successors (iblock)
  (next (end iblock)))

(defclass function (dynamic-environment value)
  (;; The starting iblock of the function. NIL if there are no iblocks
   ;; in the function. This can happen when all the function's iblocks
   ;; are moved out into another function.
   (%start :initarg :start :accessor start :type (or null iblock))
   ;; The last iblock in the forward flow order.
   (%tail :initarg :tail :accessor tail :accessor end :type (or null iblock))
   ;; The return instruction of this function. If there isn't one,
   ;; i.e. the function never returns as the return is unreachable,
   ;; this is nil.
   (%returni :initarg :returni :accessor returni :type (or null returni))
   (%lambda-list :initarg :lambda-list :accessor lambda-list)
   ;; The set of variables bound by this function, i.e. the union of
   ;; the variables bound by all LETI instructions in it.
   (%variables :initarg :variables :accessor variables
               :initform (set:empty-set)
               :type set:set)
   ;; The set of catches in this function.
   (%catches :initarg :catches :accessor catches
             :initform (set:empty-set)
             :type set:set)
   ;; The set of lexicals closed over by this function. Currently
   ;; filled in by process-captured-variables.
   (%environment :initform (set:empty-set) :accessor environment
                 :type set:set)
   ;; The ENCLOSE instruction which creates this function as a
   ;; first-class value, or NIL if the function does not need to be
   ;; treated as first-class.
   (%enclose :initform nil :accessor enclose :type (or null enclose))
   ;; The set of local calls of this function.
   (%local-calls :initform (set:empty-set) :accessor local-calls
                 :type set:set)
   ;; For debug/introspection
   (%origin :initarg :origin :initform nil :reader origin)
   (%policy :initarg :policy :initform nil :reader policy)
   (%docstring :initarg :docstring :initform nil :reader docstring)
   (%original-lambda-list :initarg :original-lambda-list :initform nil
                          :reader original-lambda-list)
   (%attributes :initarg :attributes :accessor attributes
                :initform (attributes:default-attributes))
   ;; The module containing this function.
   (%module :initarg :module :reader module :type module))
  (:metaclass stealth-mixins:victim-class))

(defmethod print-object ((o function) s)
  (print-unreadable-object (o s :type t)
    (write (name o) :stream s)))

;;; A set of functions which are compiled together (as opposed to
;;; "separate compilation") and can participate in interprocedural
;;; optimizations such as inlining. For example, lexically nested
;;; functions are always compiled together.
(defclass module ()
  ((%functions :initarg :functions :accessor functions
               :initform (set:empty-set)
               :type set:set)
   (%constants :accessor constants
               :initform (set:empty-set)
               :type set:set)
   ;; FIXME: move load time value handling more to client
   (%load-time-values :accessor load-time-values
                      :initform (set:empty-set)
                      :type set:set)
   ;; This table ensures that only one constant object per similar
   ;; object is created.
   (%constant-table :accessor constant-table)))

(defmethod initialize-instance :after ((module module) &key)
  ;; FIXME: In code with file compilation semantics, we are allowed to
  ;; coalesce EQUAL constants. Figure out how to allow clients to plug
  ;; into the table initialization logic here.
  (setf (constant-table module) (make-hash-table :test #'eq)))

;;; Find the constant object for CONSTANT-VALUE in MODULE, allocating
;;; a new one in the module if necessary.
(defun constant-in-module (constant-value module)
  (let ((constant-table (constant-table module)))
    (or (gethash constant-value constant-table)
        (let ((constant (make-instance 'constant :value constant-value)))
          (set:nadjoinf (constants module) constant)
          (setf (gethash constant-value constant-table) constant)
          constant))))

;;; Find the L-T-V object for the given load-time-value form, allocating
;;; a new one in the module if necessary.
(defun load-time-value-in-module (form read-only-p module)
  ;; Actually we always make a new one. The standard mentions the possibility
  ;; of coalescing if the forms are identical, but that's pretty marginal.
  ;; We can look into it later if we need to.
  (let ((ltv (make-instance 'load-time-value
               :form form :read-only-p read-only-p)))
    (set:nadjoinf (load-time-values module) ltv)
    ltv))

;;; The set of blocks in a function that have nonlocal entrances.
(defmethod entrances ((function function))
  (let ((entrances (set:empty-set)))
    (set:doset (catch (catches function))
      (set:doset (unwind (unwinds catch))
        (set:nadjoinf entrances (destination unwind))))
    entrances))
