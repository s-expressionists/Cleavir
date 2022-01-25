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
        (policy:normalize-optimize (append optimize (optimize* env))
                                   env)
        (policy env)
        (policy:compute-policy (optimize* env) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cleavir interface
;;;

(defmethod env:variable-info ((system example) (env environment) sym)
  (let ((info (gethash sym (variables env))))
    (if (null info)
        nil
        (destructuring-bind (kind type &optional extra) info
          (ecase kind
            ((:constant)
             (make-instance 'cleavir-env:constant-variable-info
               :name sym :value extra))
            ((:special)
             (make-instance 'cleavir-env:special-variable-info
               :name sym :global-p t :type type))
            ((:macro)
             (make-instance 'cleavir-env:symbol-macro-info
               :name sym :type type :expansion extra)))))))

(defun treat-as-special-operator-p (name)
  (case name
    ((block eval-when flet function go if labels let
            let* load-time-value locally macrolet
            multiple-value-prog1 progn quote
            return-from setq symbol-macrolet tagbody the)
     t)
    ((primop:funcall primop:multiple-value-call)
     t)
    ((multiple-value-call) ; defined as a macro in macros.lisp
     nil)
    ((catch progv throw unwind-protect)
     (warn "The example system does not define the standard special operator ~s" name)
     nil)
    (otherwise nil)))

(defmethod env:function-info ((system example) (env environment) name)
  (let ((info (gethash name (functions env))))
    (if (null info)
        (if (treat-as-special-operator-p name)
            (make-instance 'env:special-operator-info :name name)
            nil)
        (destructuring-bind (kind &optional extra) info
          (ecase kind
            ((:macro)
             (make-instance 'env:global-macro-info
               :name name :expander extra))
            ((:function)
             (make-instance 'env:global-function-info
               :name name
               :type (env:parse-type-specifier 'function
                                               env system))))))))

(defmethod env:declarations ((env environment)) '())

(defmethod env:optimize-info ((env environment))
  (make-instance 'env:optimize-info
    :optimize (optimize* env) :policy (policy env)))

(defmethod env:type-expand ((env environment) typespec)
  (let* ((head (if (consp typespec) (first typespec) typespec))
         (expander (gethash head (type-expanders env))))
    (if expander
        (values (env:type-expand env (funcall expander typespec env)) t)
        (values typespec nil))))

(defmethod env:find-class (name (env environment) (system example)
                           &optional errorp)
  (cond ((gethash name (classes env)))
        (errorp (error "No class named ~s" name))
        (t nil)))

(defmethod env:eval (form env (dispatch-env environment))
  (declare (ignore env))
  ;; Ignoring the environment is incorrect, but writing an evaluator
  ;; is out of scope for this example.
  (eval form))

(defmethod env:cst-eval (cst env (dispatch-env environment)
                         (system example))
  (declare (ignore env))
  (eval (cst:raw cst)))
