(in-package #:cleavir-primop-info)

;;;; "primop" here means what other systems might call an "intrinsic"-
;;;; something with function-like behavior that the compiler handles specially
;;;; in such a way that probably doesn't involve an actual function call.
;;;; For our purposes, this means that it has a fixed number of arguments
;;;; that must be evaluated from left to right, and no other syntax.

;;;; Note that the cleavir-primop: package has many symbols that are not primops
;;;; in this particular sense, such as LET-UNINITIALIZED.

;;; structure describing a primop
(defclass info ()
  ((%name :initarg :name :reader name)
   ;; :value means it returns a value. :effect means it doesn't.
   ;; An integer means it's a conditional with that many branches.
   (%out-kind :initarg :out-kind :reader out-kind
              :type (or (member :value :effect) (integer 2)))
   ;; Number of inputs accepted
   (%ninputs :initarg :ninputs :reader ninputs :type (integer 0))
   ;; Primop-specific arguments
   (%arguments :initarg :arguments :reader arguments :initform nil)
   ;; Miscellaneous attributes
   (%attributes :initarg :attributes :reader attributes
                :initform (cleavir-attributes:default-attributes))))

(defmethod print-object ((o info) s)
  (print-unreadable-object (o s :type t)
    (write (name o) :stream s))
  o)

(defmethod make-load-form ((o info) &optional env)
  (make-load-form-saving-slots o :environment env))

(defvar *primops* (make-hash-table :test #'equal))

(defun info (name)
  (or (gethash name *primops*)
      (when (consp name) ; name and arguments
        (let ((proto (gethash (car name) *primops*)))
          (if proto
              (setf (gethash name *primops*)
                    (make-instance 'info
                      :name (car name)
                      :attributes (attributes proto)
                      :out-kind (out-kind proto)
                      :ninputs (ninputs proto)
                      :arguments (rest name)))
              nil)))
      (error "BUG: No primop: ~a" name)))

(defmacro defprimop (name ninputs out &rest flags)
  `(setf (gethash ',name *primops*)
         (make-instance 'info
           :name ',name
           :attributes (make-instance 'cleavir-attributes:attributes
                         :flags (cleavir-attributes:make-flags ,@flags))
           :out-kind ',out :ninputs ',ninputs)))
