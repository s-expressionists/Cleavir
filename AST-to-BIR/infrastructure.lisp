(in-package #:cleavir-ast-to-bir)

(defvar *variables*)
(defvar *block-info*)
(defvar *go-info*)
(defvar *function-info*)

;;; KLUDGE: This seems necessary to reconstruct the lexicality information CST-to-AST
;;; destroys. Should probably be done more cleanly there.
;;; Essentially we make sure that any variables whose binders were set in an inner
;;; function get reset to this outer function.
(defun fix-binder (variable new-binder)
  (check-type new-binder cleavir-bir:function)
  (let ((binder (cleavir-bir:binder variable)))
    (when (and (typep binder 'cleavir-bir:function)
               (typep new-binder 'cleavir-bir:function))
      (loop with function
            for encloses = (cleavir-bir:encloses binder)
              then (cleavir-bir:encloses function)
            for nencloses = (cleavir-set:size encloses)
            when (zerop nencloses)
              do (loop-finish)
            do (assert (= 1 nencloses))
               (setf function (cleavir-bir:function (cleavir-set:arb encloses)))
            when (eq function new-binder)
              do (setf (cleavir-bir:binder variable) new-binder)
                 (loop-finish))))
  variable)

(defun find-or-create-variable (lexical-ast binder)
  (check-type lexical-ast cleavir-ast:lexical-ast)
  (check-type binder cleavir-bir:lexical-bind)
  (fix-binder
   (or (gethash lexical-ast *variables*)
       (setf (gethash lexical-ast *variables*)
             (make-instance 'cleavir-bir:variable
               :binder binder :rtype :object)))
   binder))

(defclass inserter ()
  ((%iblock :initarg :iblock :accessor iblock)
   (%insert-point :initarg :insert-point :accessor insert-point
                  :type instruction)))

(defun function (inserter) (cleavir-bir:function (iblock inserter)))

(defun before (inserter instruction)
  (check-type inserter inserter)
  (check-type instruction cleavir-bir:instruction)
  (let ((ip (insert-point inserter)))
    (check-type ip cleavir-bir:instruction)
    (assert (null (cleavir-bir:predecessor ip)))
    (setf (cleavir-bir:predecessor ip) instruction
          (cleavir-bir:successor instruction) ip
          (cleavir-bir:iblock instruction) (iblock inserter)
          (insert-point inserter) instruction))
  instruction)

(defun finalize (inserter)
  (check-type inserter inserter)
  (setf (cleavir-bir:start (iblock inserter)) (insert-point inserter))
  (slot-makunbound inserter '%insert-point))

(defun reset (inserter iblock)
  (setf (iblock inserter) iblock))

(defun terminate (inserter terminator)
  (check-type inserter inserter)
  (check-type terminator cleavir-bir:terminator)
  (let ((i (iblock inserter)))
    (loop for next in (cleavir-bir:next terminator)
          do (cleavir-set:nadjoinf (cleavir-bir:predecessors next) i))
    (setf (cleavir-bir:start i) terminator
          (cleavir-bir:end i) terminator
          (cleavir-bir:iblock terminator) i
          (insert-point inserter) terminator)))

(defun make-iblock (inserter
                    &key (function (function inserter))
                      (dynamic-environment
                       (cleavir-bir:dynamic-environment (iblock inserter))))
  (let ((ib
          (make-instance 'cleavir-bir:iblock
            :function function :dynamic-environment dynamic-environment)))
    (cleavir-set:nadjoinf (cleavir-bir:iblocks function) ib)
    ib))

(defun adjoin-variable (inserter variable)
  (check-type inserter inserter)
  (check-type variable cleavir-bir:variable)
  (cleavir-set:nadjoinf (cleavir-bir:variables (function inserter))
                        variable)
  (values))

;; MVALUES is a datum with rtype :multiple-values.
(defun figure-mvalues (inserter mvalues context)
  (case context
    (:multiple-values (list mvalues))
    (:effect (values))
    (t
     (multiple-value-bind (mtf outputs)
         (cleavir-bir:make-multiple-to-fixed mvalues context)
       (before inserter mtf)
       outputs))))

;; CONTEXT is a fixed-values context.
(defun figure-n-values (inserter inputs context)
  (declare (ignore inserter))
  (let ((linputs (length inputs)) (lcontext (length context)))
    (cond ((= lcontext linputs) inputs)
          ((< lcontext linputs) (subseq inputs 0 lcontext))
          (t (append inputs
                     (loop repeat (- lcontext linputs)
                           collect (cleavir-bir:make-constant 'nil)))))))

;;; Given a datum, which may be a computation, and a compile-ast context,
;;; insert fixed-to-multiple or whatever else to make it match the context,
;;; insert the computation if it is a computation, and return the results.
(defun return-1 (inserter datum context)
  (prog1
      (case context
        (:multiple-values
         (list (before inserter (make-instance 'cleavir-bir:fixed-to-multiple
                                  :inputs (list datum)))))
        (:effect (values))
        (t (figure-n-values inserter (list datum) context)))
    (when (typep datum 'cleavir-bir:instruction)
      (before inserter datum))))

;;; This is used internally in ast-to-hir for when a form
;;; that was compiled in a value context never returns, e.g. due to an
;;; unreachable or return-from.
(defclass dummy (cleavir-bir:datum) ())

(defun no-return (context)
  (if (effect-context-p context)
      (values)
      (make-instance 'dummy)))

;;; A context is either:
;;; a sequence of rtypes, indicating values to be returned, or
;;; :multiple-values, or
;;; :effect, indicating any value computed will be discarded.
;;; Currently only :object rtypes are allowed.

;;; A side-effects-only context; value discarded
(defun effect-context-p (context)
  (eq context :effect))

(defun context-p (object)
  (or (effect-context-p object)
      (eq object :multiple-values)
      (and (typep object 'sequence)
           (every (lambda (x) (typep x 'cleavir-bir:rtype)) object))))

(defgeneric compile-function (ast))

(defun compile-toplevel (ast)
  (let ((*variables* (make-hash-table :test #'eq))
        (*block-info* (make-hash-table :test #'eq))
        (*go-info* (make-hash-table :test #'eq))
        (*function-info* (make-hash-table :test #'eq)))
    (compile-function ast)))

;;; Returns a list of data.
;;; If multiple-values, a one element list with an element that's a datum
;;; with rtype = :multiple-values.
(defgeneric compile-ast (ast inserter rtype))

(defgeneric compile-test-ast (ast inserter next))
