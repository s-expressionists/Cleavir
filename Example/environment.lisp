(in-package #:cleavir-example)

;;;; We define here a global environment for Cleavir to refer to.
;;;; It includes all the functions and classes in the host (i.e. the
;;;; Lisp implementation you're loading this in). It does _not_
;;;; include macros, because those may expand into implementation-
;;;; dependent code that Cleavir would have to be taught to process.
;;;; A few macros for this environment are defined in macros.lisp,
;;;; but not every standard macro.

(defclass environment ()
  ((%variables :initform (make-hash-table) :reader variables
               :type hash-table)
   (%functions :initform (make-hash-table :test #'eq)
               :reader functions :type hash-table)
   (%classes :initform (make-hash-table)
             :reader classes :type hash-table)
   (%type-expanders :initform (make-hash-table)
                    :reader type-expanders :type hash-table)
   (%optimize :initform '((safety 1) (debug 1) (speed 1)
                          (space 1) (compilation-speed 1))
              :accessor optimize* :type list)
   (%policy :accessor policy :type list)))

(defvar *environment* (make-instance 'environment)
  "The \"global\" environment used by the example compiler.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic definitions
;;;

(defun %defspecial (name env)
  (setf (gethash name (variables env)) `(:special t)))
(defun %defconstant (name value env)
  (setf (gethash name (variables env)) `(:constant t ,value)))
(defun %defsmacro (name expansion env)
  (setf (gethash name (variables env)) `(:macro t ,expansion)))
(defun proclaim-vartype (name type env)
  (let ((info (gethash name (variables env))))
    (if info
        (setf (second info) `(and ,(second info) ,type))
        nil)))

(defun %defun (name env)
  (setf (gethash name (functions env)) `(:function)))
(defun %defmacro (name macrofun env)
  (setf (gethash name (functions env)) `(:macro ,macrofun)))

(defun %defclass (name class env)
  (setf (gethash name (classes env)) class))

(defun proclaim-optimize (optimize env)
  (setf (optimize* env)
        (policy:normalize-optimize nil (append optimize (optimize* env)))
        (policy env)
        (policy:compute-policy nil (optimize* env))))
