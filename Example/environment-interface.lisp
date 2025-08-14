(in-package #:cleavir-example)

;;;; This file includes definitions of the CLEAVIR-ENVIRONMENT methods
;;;; required for CST-to-AST to work with the example environment
;;;; defined in environment.lisp.
;;;; Note that the way the environment stores the information can be
;;;; completely distinct from the query objects these methods return.
;;;; These generic functions are just an interface so that Cleavir can
;;;; understand an arbitrary environment representation.

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
    ((primop:funcall primop:multiple-value-call primop:ast primop:truly-the)
     t)
    ;; Defined as macros in macros.lisp
    ((catch multiple-value-call progv throw unwind-protect)
     nil)
    (otherwise nil)))

(defvar *folds* (make-hash-table :test #'equal)) ; filled in fold.lisp

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
             (let ((fold (gethash name *folds*)))
               (make-instance 'env:global-function-info
                 :name name
                 :type (env:parse-type-specifier 'function
                                                 env system)
                 :attributes (if fold
                                 (make-instance 'attributes:attributes
                                   :identities (list name))
                                 (attributes:default-attributes))))))))))

(defmethod env:declarations ((sys example) (env environment)) '())

(defmethod env:optimize-info ((sys example) (env environment))
  (make-instance 'env:optimize-info
    :optimize (optimize* env) :policy (policy env)))

(defmethod env:type-expand ((system example) (env environment) typespec)
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

(defmethod env:eval (form env (system example))
  (declare (ignore env))
  ;; Ignoring the environment is incorrect, but writing an evaluator
  ;; is out of scope for this example.
  (eval form))

(defmethod env:cst-eval (cst env (system example))
  (declare (ignore env))
  (eval (cst:raw cst)))

;;; KLUDGE: CTYPE doesn't let us specify the type expander very well.
(defmethod env:parse-compound-type-specifier (head rest env (sys example))
  (ctype:specifier-ctype (cons head rest)))
(defmethod env:parse-expanded-type-specifier (tspec env (sys example))
  (ctype:specifier-ctype tspec))
