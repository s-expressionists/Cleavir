(in-package #:cleavir-bir)

(defclass dynamic-environment ()
  ((%scope :initarg :scope :accessor scope :initform (set:empty-set)
           :type set:set
           :documentation "The set of iblocks that have this as their dynamic environment."))
           (:documentation "Abstract. Something that can serve as a dynamic environment.
This is a dynamic environment in the sense of CLHS 3.1.1.2. An equivalent characterization is that it is a property of the continuation.
Dynamic environments include dynamic variable bindings and exit points, for example."))

(defun parent (dynamic-environment)
  "Return the parent dynamic environment of this dynamic environment. If this dynamic environment is a function, its parent is conceptually variable, and NIL is returned."
  (if (typep dynamic-environment 'function)
      nil
      (dynamic-environment (iblock dynamic-environment))))

(defgeneric origin (bir)
  (:documentation "Return the source position of a BIR object. The nature of this source position is up to the producer of the BIR."))

(defclass datum ()
  ((%name :initarg :name :initform nil :reader name
          :type (or symbol (cons symbol (cons symbol null)) ; e.g. (LABELS REC)
                    null)
          :documentation "A name for debugging and display."))
  (:documentation "Abstract. A representation of zero or more runtime values. In some cases, data may represent non-Lisp objects, such as \"unboxed\" objects, or low-level objects like pointers."))

(defclass lexical (datum) ()
  (:documentation "Abstract. A datum that can be bound in a lexical environment.

See VARIABLE
See COME-FROM"))

(defmethod print-object ((o datum) stream)
  (print-unreadable-object (o stream :type t :identity t)
    ;; NAME is always bound normally, but this safety is useful when debugging.
    (when (slot-boundp o '%name)
      (let ((name (name o)))
        (when name (write name :stream stream))))))

(defgeneric unused-p (datum)
  (:documentation "Return true iff the datum is unused."))
(defgeneric ssa-p (datum)
  (:documentation "Return true iff the datum has exactly one definition (e.g., is a variable that is bound and then not set again). Note that a datum that has only one definition in a program's source is SSA even if that definition can be reached repeatedly while executing the program."))

;;; A datum with only one definition (static single assignment).
(defclass ssa (datum) ()
  (:documentation "Abstract. A datum with only one definition. SSA stands for \"static single assignment\"."))
(defmethod ssa-p ((ssa datum)) t)
(defgeneric ctype (linear-datum)
  (:documentation "The type of the datum that we can assume when making inferences.

See ASSERTED-TYPE"))
(defgeneric asserted-type (linear-datum)
  (:documentation "The type of the datum that the code declares. While it would be unsafe to use this for inference, it can be used for example to report statically detectable type errors.

See CTYPE"))

;;; Default for ctypes.
(defvar *top-ctype*)
(defun current-top-ctype ()
  (if (boundp '*top-ctype*)
      *top-ctype*
      (ctype:values-top nil)))

(defgeneric attributes (object)
  (:documentation "Retrieve flow information for OBJECT beyond its type."))

(defclass linear-datum (datum)
  ((%use :initarg :use :initform nil :reader use :accessor %use
         :type (or null instruction))
   (%asserted-type :initform (current-top-ctype)
                   :initarg :asserted-type
                   :accessor asserted-type
                   :documentation "A type that the code declares holds for this linear-datum.")
   (%derived-type :initform (current-top-ctype)
                  :initarg :derived-type
                  :writer (setf derived-type)
                  ;; For a generic linear datum, the type we use to
                  ;; make inferences is just the type the compiler has
                  ;; proven about this datum.
                  :reader ctype
                  :documentation "A type the compiler has proven holds for this linear-datum.")
   (%attributes :initarg :attributes :accessor attributes
                :initform (attributes:default-attributes)
                :documentation "Additional flow attributes"))
  (:documentation "Abstract. A datum with only one use (statically). Note that a datum with only one use in a program's source is linear, even if that use can be reached multiple times during execution of the program."))
(defmethod unused-p ((datum linear-datum))
  (null (use datum)))

(defclass transfer (ssa linear-datum)
  ()
  (:documentation "Abstract. A datum with exactly one definition and exactly one use.

See SSA
See LINEAR-DATUM"))

(defclass value (ssa)
  ()
  (:documentation "Abstract. An SSA datum with only one definition - itself. Used for e.g. constants.

See SSA"))

(defclass constant (value)
  ((%value :initarg :value :reader constant-value)
   (%readers :initform (set:empty-set) :accessor readers))
  (:documentation "A datum representing a constant.
For linearity purposes, constants cannot be used directly as inputs to most instructions, and must instead go through CONSTANT-REFERENCE or the like first.

See CONSTANT-REFERENCE"))

(defclass function-cell (value)
  (;; %FUNCTION-NAME is a separate slot from %NAME, even though they are
   ;; usually identical, to emphasize that they don't have to be, and that
   ;; the %NAME is only for debugging.
   (%function-name :initarg :function-name :reader function-name)
   (%readers :initform (set:empty-set) :accessor readers))
  (:documentation "A datum representing a function cell.
A function cell is an implementation defined object that is the identity of a function definition, so for example (setf fdefinition) on the name does alter the cell to refer to the new function. This separates the definition from the name, which is useful for e.g. allowing the same name to refer to different functions in different global environments.
If an implementation does not have special function cells, it could just use the function name as a cell.
Cleavir marks cells as different from other constants so that they may be resolved specially by an implementation's linking loader.

See CONSTANT-FDEFINITION"))

(defclass variable-cell (value)
  ((%variable-name :initarg :variable-name :reader variable-name)
   (%readers :initform (set:empty-set) :accessor readers))
  (:documentation "A datum representing a variable cell.
A variable cell is an implementation defined object that is the identity of a dynamic variable definition, so for example makunbound followed by (setf symbol-value) on the name does alter the cell to refer to the new value. This separates the definition from the name, which is useful for e.g. allowing the same name to refer to different functions in different global environments.
If an implementation does not have special variable cells, it could just use the variable name as a cell.
Cleavir marks cells as different from other constants so that they may be resolved specially by an implementation's linking loader.

See CONSTANT-SYMBOL-VALUE
See SET-CONSTANT-SYMBOL-VALUE
See CONSTANT-BIND"))

(defmethod print-object ((object constant) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (constant-value object) :stream stream)))

;;; FIXME: move load time value handling more to client
(defclass load-time-value (value)
  ((%form :initarg :form :reader form)
   (%read-only-p :initarg :read-only-p :reader read-only-p)
   (%readers :initform (set:empty-set) :accessor readers))
  (:documentation "A datum representing a cl:load-time-value form.
For linearity purposes, LOAD-TIME-VALUE data cannot be used directly as inputs to instructions except for LOAD-TIME-VALUE-REFERENCE.

See LOAD-TIME-VALUE-REFERENCE"))

;;; These variables are used for defaulting the origin and policy.
;;; If they are not bound it should still be possible to make instructions,
;;; however; default values are NIL. (TODO: Is that right for policy?)
(defvar *policy*)
(defvar *origin*)
(defun current-policy () (if (boundp '*policy*) *policy* nil))
(defun current-origin () (if (boundp '*origin*) *origin* nil))

(defgeneric inputs (instruction)
  (:documentation "Return an instruction's input data as a sequence. Direct modification of the sequence is not permitted. Go through (SETF INPUTS) instead, or higher level operators.

See DATUM
See REPLACE-USES"))
(defgeneric (setf inputs) (new-inputs instruction)
  (:documentation "Set an instruction's input data."))
(defgeneric outputs (instruction)
  (:documentation "Return an instruction's output data as a sequence. Direct modification of the sequence is not permitted. In almost all cases, the list will have zero or one outputs."))
(defgeneric (setf outputs) (new-outputs instruction)
  (:documentation "set an instruction's output data."))

(defclass instruction ()
  ((%predecessor :initarg :predecessor :accessor predecessor
                 :initform nil
                 :type (or instruction null)
                 :documentation "The previous instruction in its IBLOCK. If NIL, this is the head instruction of the IBLOCK.")
   (%successor :initarg :successor :accessor successor
               ;; FIXME: This type declaration causes crashes-to-LDB in SBCL
               ;; under unclear circumstances.
               :type #-sbcl (or instruction null)
                     #+sbcl (or standard-object null)
               :documentation "The next instruction. If NIL, this instruction terminates an iblock.")
   (%inputs :initform nil :initarg :inputs :accessor inputs
            ;; Sequence of DATA.
            :type sequence)
   (%outputs :initform '() :initarg :outputs :accessor outputs
             :type sequence)
   (%iblock :initarg :iblock :accessor iblock :type iblock
            :documentation "The IBLOCK this instruction belongs to.")
   (%policy :initform (current-policy) :initarg :policy :reader policy)
   (%origin :initform (current-origin) :initarg :origin :reader origin)
   (%should-process :initform T
                    :initarg :should-process
                    :accessor :should-process
                    :type boolean
                    :documentation "Determines whether to process this instruction on the next pass during meta-evaluation."))
  (:documentation "Abstract. Cleavir's representation of a computation to be done.
All instructions have a sequence of input data and a sequence of output data. With a few exceptions, documented in individual instruction classes, all inputs and outputs are linear.

See INPUTS
See OUTPUTS
See SUCCESSOR
See IBLOCK
See LINEAR-DATUM"))

(defgeneric function (ir)
  (:documentation "Return the FUNCTION this BIR object belongs to.

See FUNCTION"))
(defgeneric dynamic-environment (ir)
  (:documentation "Return the DYNAMIC-ENVIRONMENT this BIR object belongs to.

See DYNAMIC-ENVIRONMENT"))

;;; Shortcuts to get an instruction's owner
(defmethod function ((instruction instruction))
  (function (iblock instruction)))
;;; and dynamic environment.
(defmethod dynamic-environment ((instruction instruction))
  (dynamic-environment (iblock instruction)))

(defgeneric definitions (datum)
  (:documentation "Return the set of definitions of a DATUM."))

(defclass output (transfer)
  ((%definition :initform nil :initarg :definition
                :reader definition :accessor %definition))
  (:documentation "A DATUM output by an instruction. All instructions output only OUTPUTs, except for terminators which output PHIs, and WRITEVAR which outputs a VARIBLE."))

(defmethod definitions ((datum output))
  (set:make-set (definition datum)))

(defmethod origin ((datum output))
  (when (definition datum) (origin (definition datum))))

;;; some useful mixins
(defclass no-input (instruction)
  ((%inputs :initform nil :type null))
  (:documentation "Mixin. An instruction with no inputs."))
(defclass one-input (instruction)
  ((%inputs :initform nil
            :type (or null (cons datum null))))
  (:documentation "Mixin. An instruction with exactly one input."))
(defclass no-output (instruction)
  ((%outputs :initform '() :type null))
  (:documentation "Mixin. An instruction with no outputs."))
(defclass one-output (instruction)
  ((%outputs :initform nil
             :type (or null (cons datum null))))
  (:documentation "Mixin. An instruction with exactly one output."))

(defgeneric input (instruction)
  (:method ((inst one-input)) (first (inputs inst)))
  (:documentation "Shortcut to get the sole input of an instruction."))
(defgeneric output (instruction)
  (:method ((inst one-output)) (first (outputs inst)))
  (:documentation "Shortcut to get the sole output of an instruction."))

(defclass terminator (instruction)
  ((%successor :initform nil :type null)
   (%next :initarg :next :accessor next
          ;; A list of iblocks.
          :type list
          :documentation "The list of IBLOCKs this terminator can branch to."))
  (:documentation "Abstract. An instruction that terminates (is at the end of) an IBLOCK."))

(defclass terminator0 (terminator)
  ((%next :initform nil :type null))
  (:documentation "Abstract. A TERMINATOR with no NEXT IBLOCKs. This means that is an end to execution in this function."))

(defclass terminator1 (terminator)
  ((%next :type (or null (cons iblock null)))) ; can be null after metaevaluate
  (:documentation "Abstract. A TERMINATOR with exactly one NEXT IBLOCK. After this instruction, control unconditionally transfers to that IBLOCK."))

(defclass argument (value transfer)
  ((%function :initarg :function :reader function :type function))
  (:documentation "A DATUM representing an argument to a FUNCTION."))

;;; An ARGUMENT is unused if either it itself has no use or it's use
;;; is a LETI with no readers.
(defmethod unused-p ((datum argument))
  (or (call-next-method)
      (let ((use (use datum)))
        (and (typep use 'leti) (unused-p (output use))))))

(defclass phi (linear-datum)
  ((%iblock :initarg :iblock :reader iblock
            :type iblock))
  (:documentation "A DATUM representing an argument to an IBLOCK. Alternatively, it may be characterized as a data merger point, as in conventional SSA form."))

(defmethod definitions ((phi phi))
  (let ((ib (iblock phi))
        (definitions (set:empty-set)))
    (set:doset (predecessor (predecessors ib))
      (let ((end (end predecessor)))
        (unless (typep end 'come-from)
          (set:nadjoinf definitions end))))
    (set:doset (entrance (entrances ib))
      (set:nadjoinf definitions (end entrance)))
    definitions))

(defun transitive-use (linear-datum)
  "The eventual use of a datum after considering its transfer through JUMP instructions.

See JUMP"
  (loop
    (let ((use (use linear-datum)))
      (unless (typep use 'jump)
        (return use))
      (setq linear-datum
            (nth (position linear-datum (inputs use))
                 (outputs use))))))

(defun phi-inputs (phi)
  "Return the set of DATA that can provide values for this PHI."
  (let* ((ib (iblock phi))
         (pos (position phi (inputs ib)))
         (inputs (set:empty-set)))
    (assert (not (null pos)))
    (set:doset (predecessor (predecessors ib))
      (let ((end (end predecessor)))
        (unless (typep end 'come-from)
          (let ((in (nth pos (inputs end))))
            (assert (not (null in)))
            (set:nadjoinf inputs in)))))
    (set:doset (entrance (entrances ib))
      (let* ((end (end entrance))
             (in (nth pos (inputs end))))
        (assert (not (null in)))
        (set:nadjoinf inputs in)))
    inputs))

(defclass variable (lexical)
  ((%extent :initarg :extent :accessor extent
            :initform :unanalyzed
            :type (member :unanalyzed
                          :local
                          :dynamic
                          :indefinite)
            :documentation "Indicates the extent of a lexical variable. Filled in by
   variable extent analysis. Note that the dynamic extent of
   variables themselves are induced by the extent of the closures
   closing over it and independent of dynamic extent
   declarations. A dynamic extent declaration on a variable is
   instead supposed to mark the extent of the value it is bound
   to. Therefore we carry that kind of dynamic extent on the binder
   of the variable and propagate that back to implement the
   semantics of dynamic extent declarations.")
   (%binder :initarg :binder :accessor binder :type leti
            :documentation "The LETI that binds this variable.")
   (%writers :accessor writers
             :initform (set:empty-set)
             ;; All WRITEVAR instructions.
             :type set:set)
   (%readers :accessor readers
             :initform (set:empty-set)
             ;; All READVAR instructions.
             :type set:set)
   (%use-status :initarg :use-status :initform nil :reader use-status
                :type (member nil set read)
                :documentation "Indication of how this variable has been used, for the purpose of cl:ignore-related warnings.")
   (%ignore :initarg :ignore :reader ignore
            ;; TODO: Clarify type
            :documentation "The cl:ignore declaration on this variable."))
  (:documentation "A DATUM representing a mutable lexical variable.
It can be read from and written to any number of times, and across FUNCTIONs, but these writes and reads must be mediated by WRITEVAR and READVAR instructions."))

(defmethod origin ((datum variable)) (origin (binder datum)))

(defmethod unused-p ((datum variable))
  (set:empty-set-p (readers datum)))

(defun record-variable-set (variable)
  "Mark that the program writes the given variable."
  (with-slots (%use-status) variable
    (or %use-status (setf %use-status 'set))))

(defun record-variable-ref (variable)
  "Mark that the program reads from the given variable."
  (with-slots (%use-status) variable
    (setf %use-status 'read)))

(defmethod function ((v variable))
  (function (binder v)))

(defun immutablep (variable)
  "Return true iff this variable has only one definition statically."
  (= (set:size (writers variable)) 1))

(defun closed-over-p (variable)
  "Return true iff this variable is shared across multiple FUNCTIONs."
  (let ((owner (function variable)))
    (set:doset (reader (readers variable))
      (unless (eq owner (function reader))
        (return-from closed-over-p t)))
    (set:doset (writer (writers variable))
      (unless (eq owner (function writer))
        (return-from closed-over-p t)))))

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
   (%entrances :initarg :entrances :accessor entrances
               :initform (set:empty-set)
               :type set:set
               :documentation "The set of IBLOCKs that can transfer control to this iblock nonlocally, i.e. with an UNWIND operation.")
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
   (%name :initarg :name :reader name :initform nil)
   (%should-process :initform T
                    :initarg :should-process
                    :accessor :should-process
                    :type boolean
                    :documentation "Determines whether to process this iblock on the next pass during meta-evaluation."))
  (:documentation "A sequence of instructions with no branching.
In other words this is a conventional \"basic block\", except that Cleavir will sometimes keep distinct iblock segments around for various purposes, such as to indicate differnet dynamic environments."))

(defmethod print-object ((o iblock) s)
  (print-unreadable-object (o s :type t)
    (write (name o) :stream s)))

(defgeneric startedp (ir)
  (:documentation "Return true iff the IR has begun being generated, i.e. has any instructions in it.")
  (:method ((iblock iblock))
    (slot-boundp iblock '%start)))

(defgeneric terminatedp (ir)
  (:documentation "Return true iff the IR has a terminator.")
  (:method ((iblock iblock))
    (slot-boundp iblock '%end)))

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
   ;; The set of come-froms in this function.
   (%come-froms :initarg :come-froms :accessor come-froms
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
   ;; Other uses of this function, e.g. THE or UNWIND-PROTECT.
   (%other-uses :initform (set:empty-set) :accessor other-uses
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
   (%module :initarg :module :reader module :type module)
   (%should-process :initform T
                    :initarg :should-process
                    :accessor :should-process
                    :type boolean
                    :documentation "Determines whether to process this function on the next pass during meta-evaluation."))
  (:documentation "Cleavir's representation of the code for a Lisp function."))

(defmethod print-object ((o function) s)
  (print-unreadable-object (o s :type t)
    (write (name o) :stream s)))

(defclass module ()
  ((%functions :initarg :functions :accessor functions
               :initform (set:empty-set)
               :type set:set)
   (%constants :accessor constants
               :initform (set:empty-set)
               :type set:set)
   ;; This table ensures that only one constant object per similar
   ;; object is created.
   (%constant-table :accessor constant-table)
   (%function-cell-table :accessor function-cell-table)
   (%variable-cell-table :accessor variable-cell-table))
  (:documentation "A set of functions which are compiled together (as opposed to \"separate compilation\") and which can participate in interprocedural optimizations such as inlining.
 For example, lexically nested functions are always compiled together."))

(defmethod initialize-instance :after ((module module) &key)
  ;; FIXME: In code with file compilation semantics, we are allowed to
  ;; coalesce EQUAL constants. Figure out how to allow clients to plug
  ;; into the table initialization logic here.
  (setf (constant-table module) (make-hash-table :test #'eq)
        (function-cell-table module) (make-hash-table :test #'equal)
        (variable-cell-table module) (make-hash-table :test #'eq)))

(defun constant-in-module (constant-value module)
  "Find the CONSTANT for the given value in MODULE, allocating a new one in the module if necessary.

See CONSTANT"
  (let ((constant-table (constant-table module)))
    (or (gethash constant-value constant-table)
        (let ((constant (make-instance 'constant :value constant-value)))
          (set:nadjoinf (constants module) constant)
          (setf (gethash constant-value constant-table) constant)
          constant))))

(defun load-time-value-in-module (form read-only-p module)
  "Find the L-T-V object for the given cl:load-time-value form, allocating a new one in the module if necessary."
  ;; Actually we always make a new one. The standard mentions the possibility
  ;; of coalescing if the forms are identical, but that's pretty marginal.
  ;; We can look into coalescence later if we need to.
  (let ((ltv (make-instance 'load-time-value
               :form form :read-only-p read-only-p)))
    (set:nadjoinf (constants module) ltv)
    ltv))

(defun function-cell-in-module (function-name module)
  "Find the FUNCTION-CELL for the given name in MODULE, or allocate a new one in the module if necessary.

See FUNCTION-CELL"
  (let ((table (function-cell-table module)))
    (or (gethash function-name table)
        (let ((fcell (make-instance 'function-cell
                       :name function-name :function-name function-name)))
          (set:nadjoinf (constants module) fcell)
          (setf (gethash function-name table) fcell)
          fcell))))

(defun variable-cell-in-module (variable-name module)
  "Find the VARIABLE-CELL for the given name in MODULE, or allocate a new one in the module if necessary.

See VARIABLE-CELL"
  (let ((table (variable-cell-table module)))
    (or (gethash variable-name table)
        (let ((vcell (make-instance 'variable-cell
                       :name variable-name :variable-name variable-name)))
          (set:nadjoinf (constants module) vcell)
          (setf (gethash variable-name table) vcell)
          vcell))))

(defmethod entrances ((function function))
  "The set of blocks in a function that have nonlocal entrances."
  (let ((entrances (set:empty-set)))
    (set:doset (come-from (come-froms function))
      (set:doset (unwind (unwinds come-from))
        (set:nadjoinf entrances (destination unwind))))
    entrances))
