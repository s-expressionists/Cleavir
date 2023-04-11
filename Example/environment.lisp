(in-package #:cleavir-example)

;;;; We define here a global environment for Cleavir to refer to.
;;;; It includes all the functions and classes in the host (i.e. the
;;;; Lisp implementation you're loading this in). It does _not_
;;;; include macros, because those may expand into implementation-
;;;; dependent code that Cleavir would have to be taught to process.
;;;; A few macros for this environment are defined in macros.lisp,
;;;; but not every standard macro.

(defclass environment (trucler-reference:environment)
  ((%type-expanders
    :reader type-expanders
    :initform (make-hash-table)
    :type hash-table)
   (%optimize
    :accessor optimize*
    :initform '((speed 1) (compilation-speed 1)
                (debug 1) (space 1) (safety 1))
    :type list)
   (%policy :accessor policy :type list))
  (:default-initargs
   :global-environment (make-instance 'clostrum-basic:run-time-environment)))

(defvar *environment* (make-instance 'environment)
  "The \"global\" environment used by the example compiler.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic definitions
;;;

(defun %defspecial (name init-p value environment)
  ;; FIXME(paul) What value to initialize this to if INIT-P is NIL?
  ;; This function is not actually even used.
  (setf (env:special-variable *client*
                              (trucler:global-environment *client* environment)
                              name init-p)
        value))

(defun %defconstant (name value environment)
  (setf (env:constant-variable *client*
                               (trucler:global-environment *client* environment)
                               name)
        value))

(defun %defsmacro (name expansion environment)
  (setf (env:symbol-macro *client*
                          (trucler:global-environment *client* environment)
                          name)
        expansion))

(defun proclaim-vartype (name type environment)
  (setf (env:variable-type *client*
                           (trucler:global-environment *client* environment)
                           name)
        type))

(defun %defun (name function environment)
  (let ((global-environment (trucler:global-environment *client* environment)))
    (setf (env:fdefinition *client* global-environment name) function
          (env:function-type *client* global-environment name)
          (cst-to-ast:parse-type-specifier *client* 'function environment))))

(defun %defmacro (name expander environment)
  (setf (env:macro-function *client*
                            (trucler:global-environment *client* environment)
                            name)
        expander))

(defun %defclass (name class environment)
  (setf (env:find-class *client*
                        (trucler:global-environment *client* environment)
                        name)
        class))

(defun proclaim-optimize (optimize environment)
  (let ((optimize (policy:normalize-optimize
                   *client*
                   (append optimize (optimize* environment)))))
    (setf (optimize* environment) optimize
          (policy environment)
          (policy:compute-policy *client* optimize))))
