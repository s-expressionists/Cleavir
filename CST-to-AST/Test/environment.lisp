(cl:in-package #:cleavir-cst-to-ast-test)

(defclass environment () ())

(defmethod trucler:describe-optimize (client (environment environment))
  (make-instance 'trucler:optimize-description
    :speed 0
    :compilation-speed 0
    :debug 3
    :space 0
    :safety 3))

(defmethod trucler:describe-function
    (client (environment environment) function-name)
  (cond ((or (and (symbolp function-name)
                  (eq (symbol-package function-name)
                      (find-package 'cleavir-primop))
                  (not (eq function-name
                           'cleavir-primop:call-with-variable-bound)))
             (and (symbolp function-name)
                  (special-operator-p function-name)))
         (make-instance 'trucler:special-operator-description
           :name function-name))
        ((and (symbolp function-name)
              (eq (symbol-package function-name)
                  (find-package 'common-lisp))
              (not (null (macro-function function-name))))
         (make-instance 'trucler:global-macro-description
           :name function-name
           :expander (macro-function function-name)
           :compiler-macro nil))
        ((and (symbolp function-name)
              (eq (symbol-package function-name)
                  (find-package 'common-lisp))
              (typep (ignore-errors (fdefinition function-name)) 'function))
         (make-instance 'trucler:global-function-description
           :name function-name
           :dynamic-extent nil
           :inline-data nil
           :ignore nil
           :compiler-macro nil
           :inline nil
           :type t))
        (t nil)))

(defmethod trucler:describe-variable (client (environment environment) symbol)
  (if (member symbol '(*special1* *special2*))
      (make-instance 'trucler:global-special-variable-description
        :ignore nil
        :name symbol)
      nil))

(defmethod cst-to-ast:eval (form (env1 environment) (env2 environment))
  (eval form))
