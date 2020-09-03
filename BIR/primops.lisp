(in-package #:cleavir-bir)

;;;; "primop" here means what other systems might call an "intrinsic"-
;;;; something with function-like behavior that the compiler handles specially
;;;; in such a way that probably doesn't involve an actual function call.
;;;; For our purposes, this means that it has a fixed number of arguments
;;;; that must be evaluated from left to right, and no other syntax or
;;;; properties at the IR level.

;;; structure describing a primop
(defclass primop-info ()
  (;; rtype of the return, or NIL if it doesn't return a value
   (%rtype :initarg :rtype :reader rtype :type (or rtype null))
   ;; List of rtypes of the inputs
   (%in-rtypes :initarg :in-rtypes :reader in-rtypes :type list)))

(defmethod make-load-form ((o primop-info) &optional env)
  (make-load-form-saving-slots o :environment env))

(defvar *primops* (make-hash-table :test #'eq))

(macrolet ((defprimop (name ret &rest in)
             `(setf (gethash ',name *primops*)
                    (make-instance 'primop-info
                      :rtype ',ret
                      :in-rtypes ',in)))
           (defprimops (&rest specs)
             `(progn
                ,@(loop for spec in specs
                        collect `(defprimop ,@spec)))))
  (defprimops
      (cleavir-primop:car :object :object)
      (cleavir-primop:cdr :object :object)
    (cleavir-primop:rplaca nil :object :object)
    (cleavir-primop:rplacd nil :object :object)))
