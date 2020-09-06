(in-package #:cleavir-ast-to-bir)

(defvar *variables*)
(defvar *block-info*)
(defvar *go-info*)
(defvar *function-info*)

(defun find-or-create-variable (lexical-ast)
  (check-type lexical-ast cleavir-ast:lexical-ast)
  (or (gethash lexical-ast *variables*)
      (setf (gethash lexical-ast *variables*)
            (make-instance 'cleavir-bir:variable :rtype :object))))

(defclass inserter ()
  ((%iblock :initarg :iblock :accessor iblock)
   (%insert-point :initarg :insert-point :accessor insert-point
                  :type instruction)
   (%function :initarg :function :reader function)))

(defun before (inserter instruction)
  (check-type inserter inserter)
  (check-type instruction cleavir-bir:instruction)
  (let ((ip (insert-point inserter)))
    (check-type ip cleavir-bir:instruction)
    (assert (null (cleavir-bir:predecessor ip)))
    (setf (cleavir-bir:predecessor ip) instruction
          (cleavir-bir:successor instruction) ip
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
          do (cleavir-set:nset-adjoinf (cleavir-bir:predecessors next) i))
    (setf (cleavir-bir:start i) terminator
          (cleavir-bir:end i) terminator
          (insert-point inserter) terminator)))

(defun adjoin-variable (inserter variable)
  (check-type inserter inserter)
  (check-type variable cleavir-bir:variable)
  (cleavir-set:nset-adjoinf (cleavir-bir:variables (function inserter))
                            variable)
  (values))

(defun object-aggregate-p (rtype)
  (and (cleavir-bir:aggregatep rtype)
       (loop for i below (cleavir-bir:aggregate-length rtype)
             always (cleavir-bir:rtype= (cleavir-bir:aggregate-elt rtype i)
                                        :object))))

(defun primary-value (inserter value)
  (let ((rt (cleavir-bir:rtype value)))
    (case rt
      (:object value)
      (:multiple-values
       (let* ((mtf (make-instance 'cleavir-bir:multiple-to-fixed
                     :inputs (list value)
                     :rtype (cleavir-bir:aggregate :object)))
              (ext (make-instance 'cleavir-bir:extract
                     :index 0
                     :inputs (list mtf)
                     :rtype :object)))
         (before inserter ext)
         (before inserter mtf)
         ext))
      (t
       (assert (object-aggregate-p rt))
       (if (zerop (cleavir-bir:aggregate-length rt))
           (cleavir-bir:make-constant nil)
           (before inserter
                   (make-instance 'cleavir-bir:extract
                     :index 0 :inputs (list value) :rtype :object)))))))

(defun multiple-values (inserter value)
  (let ((rt (cleavir-bir:rtype value)))
    (case rt
      (:multiple-values value)
      (:object
       (let* ((create (make-instance 'cleavir-bir:create
                        :inputs (list value)
                        :rtype '#.(cleavir-bir:aggregate :object)))
              (ftm (make-instance 'cleavir-bir:fixed-to-multiple
                     :inputs (list create))))
         (before inserter ftm)
         (before inserter create)
         ftm))
      (t
       (assert (object-aggregate-p rt))
       (before inserter (make-instance 'cleavir-bir:fixed-to-multiple
                          :inputs (list value)))))))

(defun to-object-aggregate (inserter value agg)
  (let ((rt (cleavir-bir:rtype value)))
    (case rt
      (:multiple-values
       (before inserter (make-instance 'cleavir-bir:multiple-to-fixed
                          :inputs (list value)
                          :rtype agg)))
      (:object
       (before inserter (make-instance 'cleavir-bir:create
                          :inputs (list value)
                          :rtype agg)))
      (t
       (assert (object-aggregate-p agg))
       (let* ((target-len (cleavir-bir:aggregate-length agg))
              (source-len (cleavir-bir:aggregate-length rt)))
         (if (= target-len source-len)
             value
             (let*
                 ((shared-len (min target-len source-len))
                  (extracts
                    (loop for i below shared-len
                          collect (make-instance 'cleavir-bir:extract
                                    :index i :inputs (list value)
                                    :rtype :object)))
                  (nils
                    (loop for i from shared-len below target-len
                          collect (cleavir-bir:make-constant nil)))
                  (create
                    (make-instance 'cleavir-bir:create
                      :rtype agg :inputs (append extracts nils))))
               (before inserter create)
               (loop for e in extracts do (before inserter create))
               create)))))))

(defun figure-values (inserter value context)
  (case context
    (:multiple-values (multiple-values inserter value))
    (:object (primary-value inserter value))
    (:effect (values))
    (t
     (assert (object-aggregate-p context))
     (to-object-aggregate inserter value context))))

;;; This is used internally in ast-to-hir for when a form
;;; that was compiled in a value context never returns, e.g. due to an
;;; unreachable or return-from.
(defclass dummy (cleavir-bir:datum) ())

(defun no-return (context)
  (if (effect-context-p context)
      (values)
      (make-instance 'dummy)))

;;; A context is either:
;;; an rtype, indicating a value is to be returned, or
;;; a list of blocks, indicating a branch is expected, or
;;; :effect, indicating any value computed will be discarded

;;; A side-effects-only context; value discarded
(defun effect-context-p (context)
  (eq context :effect))

(defun one-successor-context-p (context)
  (or (effect-context-p context)
      (typep context 'cleavir-bir:rtype)))

(defun n-next-context-p (context n)
  (and (listp context) (eql (length context) n)))

(defgeneric compile-function (ast))

(defun compile-toplevel (ast)
  (let ((*variables* (make-hash-table :test #'eq))
        (*block-info* (make-hash-table :test #'eq))
        (*go-info* (make-hash-table :test #'eq))
        (*function-info* (make-hash-table :test #'eq)))
    (compile-function ast)))

(defgeneric compile-ast (ast inserter context))
