(in-package #:cleavir-bir)

;;;; "primop" here means what other systems might call an "intrinsic"-
;;;; something with function-like behavior that the compiler handles specially
;;;; in such a way that probably doesn't involve an actual function call.
;;;; For our purposes, this means that it has a fixed number of arguments
;;;; that must be evaluated from left to right, and no other syntax or
;;;; properties at the IR level.

;;; structure describing a primop
(defclass primop-info ()
  ((%name :initarg :name :reader name)
   ;; List of rtypes of the outputs, or an integer.
   ;; If the latter, indicates that this is a test with that many branches.
   (%out-rtypes :initarg :out-rtypes :reader out-rtypes
                :type (or list (integer 2)))
   ;; List of rtypes of the inputs
   (%in-rtypes :initarg :in-rtypes :reader in-rtypes :type list)))

(defmethod make-load-form ((o primop-info) &optional env)
  (make-load-form-saving-slots o :environment env))

(defvar *primops* (make-hash-table :test #'equal))

(defun primop-info (name)
  (or (gethash name *primops*)
      (error "BUG: No primop: ~a" name)))

(defmacro defprimop (name (&rest in) out)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *primops*)
           (make-instance 'primop-info
             :name ',name
             :out-rtypes ',out :in-rtypes ',in))))

(macrolet ((defprimops (&rest specs)
             `(progn
                ,@(loop for spec in specs
                        collect `(defprimop ,@spec)))))
  (defprimops
      (cleavir-primop:car (:object) (:object))
      (cleavir-primop:cdr (:object) (:object))
    (cleavir-primop:rplaca (:object :object) ())
    (cleavir-primop:rplacd (:object :object) ())
    (symbol-value (:object) (:object))
    ((setf symbol-value) (:object :object) ())
    (fdefinition (:object) (:object))

    (cleavir-primop:fixnum-less (:object :object) 2)
    (cleavir-primop:fixnum-not-greater (:object :object) 2)
    (cleavir-primop:fixnum-equal (:object :object) 2)))
