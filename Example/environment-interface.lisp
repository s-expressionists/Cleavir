(in-package #:cleavir-example)

;;;; This file includes definitions of the Trucler and CST-to-AST
;;;; methods required for CST-to-AST to work with the example
;;;; environment defined in environment.lisp.
;;;; Note that the way the environment stores the information can be
;;;; completely distinct from the query objects these methods return.
;;;; These generic functions are just an interface so that Cleavir can
;;;; understand an arbitrary environment representation.

(defmethod trucler:add-lexical-variable
    ((client example) (environment environment) symbol &optional identity)
  (trucler:augment-with-variable-description
   client environment
   (make-instance (trucler:lexical-variable-description-class client)
     :name symbol
     :identity identity
     :type (c-ctype:top client))))

(defmethod trucler:add-local-special-variable
    ((client example) (environment environment) symbol)
  (trucler:augment-with-variable-description
   client environment
   (make-instance (trucler:local-special-variable-description-class client)
     :name symbol
     :type (c-ctype:top client))))

(defmethod trucler:add-local-symbol-macro
    ((client example) (environment environment) symbol expansion)
  (trucler:augment-with-variable-description
   client environment
   (make-instance (trucler:local-symbol-macro-description-class client)
     :name symbol
     :expansion expansion
     :type (c-ctype:top client))))

(defmethod trucler:add-local-function
    ((client example) (environment environment) symbol &optional identity)
  (trucler:augment-with-function-description
   client environment
   (make-instance (trucler:local-function-description-class client)
     :name symbol
     :identity identity
     :type (c-ctype:function-top client))))

(defun treat-as-special-operator-p (name)
  (case name
    ((block eval-when flet function go if labels let
            let* load-time-value locally macrolet
            multiple-value-prog1 progn quote
            return-from setq symbol-macrolet tagbody the)
     t)
    ((primop:funcall primop:multiple-value-call primop:ast primop:truly-the)
     t)
    ;; Defined as macros in macros.lisp
    ((catch multiple-value-call progv throw unwind-protect)
     nil)
    (otherwise nil)))

(defmethod trucler:describe-function
    ((client example) (environment environment) name)
  (let ((description (call-next-method)))
    (if (and (null description)
             (treat-as-special-operator-p name))
        (make-instance 'trucler:special-operator-description
          :name name)
        description)))

(defmethod trucler:describe-optimize
    ((client example) (environment environment))
  (apply #'make-instance 'trucler:optimize-description
         (loop for optimize in (optimize* environment)
               for (quality value) = (if (listp optimize)
                                          optimize
                                          (list optimize 3))
               collect (intern (string quality) :keyword)
               collect value)))

(defmethod trucler:describe-declarations
    ((client example) (environment environment))
  '())

(defvar *folds* (make-hash-table :test #'equal)) ; filled in fold.lisp

(defmethod cst-to-ast:attributes
    ((client example)
     (description trucler:global-function-description)
     (environment environment))
  (let* ((name (trucler:name description))
         (fold (gethash name *folds*)))
    (if fold
        (make-instance 'attributes:attributes
          :identities (list name))
        (call-next-method))))

(defmethod cst-to-ast:type-expand ((client example) typespec (env environment))
  (let* ((head (if (consp typespec) (first typespec) typespec))
         (expander (gethash head (type-expanders env))))
    (if expander
        (values (cst-to-ast:type-expand
                 client (funcall expander typespec env) env)
                t)
        (values typespec nil))))

(defmethod cst-to-ast:find-class ((client example) name (env environment)
                                  &optional errorp)
  (cond ((env:find-class client env name))
        (errorp (error "No class named ~s" name))
        (t nil)))

(defmethod cst-to-ast:eval (form env (dispatch-env environment))
  (declare (ignore env))
  ;; Ignoring the environment is incorrect, but writing an evaluator
  ;; is out of scope for this example.
  (eval form))

(defmethod cst-to-ast:cst-eval ((client example) cst env
                                (dispatch-env environment))
  (declare (ignore env))
  (eval (cst:raw cst)))

;;; KLUDGE: CTYPE doesn't let us specify the type expander very well.
(defmethod cst-to-ast:parse-compound-type-specifier
    ((client example) head rest env)
  (declare (ignore env))
  (ctype:specifier-ctype (cons head rest)))

(defmethod cst-to-ast:parse-expanded-type-specifier
    ((client example) tspec env)
  (declare (ignore env))
  (ctype:specifier-ctype tspec))
