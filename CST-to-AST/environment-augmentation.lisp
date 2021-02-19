(cl:in-package #:cleavir-cst-to-ast)

;;; Augment the environment with a single canonicalized declaration
;;; specifier.
(defgeneric augment-environment-with-declaration
    (declaration-identifier
     declaration-identifier-cst
     declaration-data-cst
     environment
     system))

(defmethod augment-environment-with-declaration
    (declaration-identifier
     declaration-identifier-cst
     declaration-data-cst
     environment
     system)
  (declare (ignore declaration-identifier-cst
                   declaration-data-cst
                   system))
  (warn "Unable to handle declarations specifier: ~s"
        declaration-identifier)
  environment)

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'dynamic-extent))
     declaration-identifier-cst
     declaration-data-cst
     environment
     system)
  (declare (ignore declaration-identifier-cst))
  (let ((var-or-function (cst:raw (cst:first declaration-data-cst))))
    (if (consp var-or-function)
        ;; (dynamic-extent (function foo))
        (trucler:add-function-dynamic-extent
         system environment (second var-or-function))
        ;; (dynamic-extent foo)
        (trucler:add-variable-dynamic-extent
         system environment var-or-function))))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'ftype))
     declaration-identifier-cst
     declaration-data-cst
     environment
     system)
  (declare (ignore declaration-identifier-cst))
  (trucler:add-function-type
   system environment (cst:raw (cst:second declaration-data-cst))
   (cleavir-env:parse-type-specifier
    (cst:raw (cst:first declaration-data-cst))
    environment system)))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'ignore))
     declaration-identifier-cst
     declaration-data-cst
     environment
     system)
  (let ((var-or-function (cst:raw (cst:first declaration-data-cst)))
        (ignore (cst:raw declaration-identifier-cst)))
    (if (consp var-or-function)
        (trucler:add-function-ignore
         system environment (second var-or-function) ignore)
        (trucler:add-variable-ignore
         system environment var-or-function ignore))))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'ignorable))
     declaration-identifier-cst
     declaration-data-cst
     environment
     system)
  (let ((var-or-function (cst:raw (cst:first declaration-data-cst)))
        (ignore (cst:raw declaration-identifier-cst)))
    (if (consp var-or-function)
        (trucler:add-function-ignore
         system environment (second var-or-function) ignore)
        (trucler:add-variable-ignore
         system environment var-or-function ignore))))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'inline))
     declaration-identifier-cst
     declaration-data-cst
     environment
     system)
  (trucler:add-inline
   system environment (cst:raw (cst:first declaration-data-cst))
   (cst:raw declaration-identifier-cst)))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'notinline))
     declaration-identifier-cst
     declaration-data-cst
     environment
     system)
  (trucler:add-inline
   system environment (cst:raw (cst:first declaration-data-cst))
   (cst:raw declaration-identifier-cst)))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'special))
     declaration-identifier-cst
     declaration-data-cst
     environment
     system)
  (declare (ignore declaration-identifier-cst))
  ;; This case is a bit tricky, because if the
  ;; variable is globally special, nothing should
  ;; be added to the environment.
  (let ((info (trucler:describe-variable
               system environment (cst:raw (cst:first declaration-data-cst)))))
    (cond ((typep info 'trucler:symbol-macro-description)
           (error 'special-symbol-macro
                  :cst (cst:first declaration-data-cst)))
          ((typep info 'trucler:global-special-variable-description)
           environment)
          (t (trucler:add-special-variable
              system environment (cst:raw (cst:first declaration-data-cst)))))))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'type))
     declaration-identifier-cst
     declaration-data-cst
     environment
     system)
  (declare (ignore declaration-identifier-cst))
  (cst:db source (type-cst variable-cst) declaration-data-cst
    (trucler:add-variable-type
     system environment (cst:raw variable-cst)
     (cleavir-env:parse-type-specifier (cst:raw type-cst)
                                       environment system))))

(defmethod augment-environment-with-declaration
    ((declaration-identifier (eql 'optimize))
     declaration-identifier-cst
     declaration-data-cst
     environment
     system)
  (declare (ignore declaration-identifier-cst declaration-data-cst
                   system))
  ;; OPTIMIZE is handled specially, so we do nothing here.
  ;; This method is just for ensuring that the default method,
  ;; which signals a warning, isn't called.
  environment)

;;; Augment the environment with an OPTIMIZE specifier.
(defun augment-environment-with-optimize (optimize environment)
  ;; Make sure every environment has a complete optimize & policy.
  (let* ((previous (cleavir-env:optimize
                    (cleavir-env:optimize-info environment)))
         (total (cleavir-policy:normalize-optimize
                 (append optimize previous)
                 environment))
         ;; Compute also normalizes, so this is slightly wasteful.
         (policy (cleavir-policy:compute-policy
                  total
                  (cleavir-env:global-environment environment))))
    (cleavir-env:add-optimize environment total policy)))

;;; Extract any OPTIMIZE information from a set of canonicalized
;;; declaration specifiers.
(defun extract-optimize (canonicalized-dspecs)
  (loop for spec in canonicalized-dspecs
        when (eq (cst:raw (cst:first spec)) 'optimize)
          append (mapcar #'cst:raw (cst:listify (cst:rest spec)))))

;;; Augment the environment with a list of canonical declartion
;;; specifiers.
(defun augment-environment-with-declarations (environment system
                                              canonical-dspecs)
  (let ((new-env
          ;; handle OPTIMIZE specially.
          (let ((optimize (extract-optimize canonical-dspecs)))
            (if optimize
                (augment-environment-with-optimize optimize environment)
                environment))))
    (loop for spec in canonical-dspecs
          for declaration-identifier-cst = (cst:first spec)
          for declaration-identifier = (cst:raw declaration-identifier-cst)
          ;; FIXME: this is probably wrong.  The data may be contained
          ;; in more than one element.  We need to wrap it in a CST or
          ;; change the interface to a-e-w-d.
          for declaration-data-cst = (cst:rest spec)
          do (setf new-env
                   (augment-environment-with-declaration
                    declaration-identifier
                    declaration-identifier-cst
                    declaration-data-cst
                    new-env
                    system)))
    new-env))

;;; Given a single variable bound by some binding form, a list of
;;; canonicalized declaration specifiers, and an environment in which
;;; the binding form is compiled, return true if and only if the
;;; variable to be bound is special.  Return a second value indicating
;;; whether the variable is globally special.
(defun variable-is-special-p (variable declarations env system)
  (let* ((existing-var-info (trucler:describe-variable system env variable))
         (global-special-var-p
           (typep existing-var-info
                  'trucler:global-special-variable-description)))
    (cond ((loop for declaration in declarations
                 thereis (and (eq (cst:raw (cst:first declaration)) 'special)
                              (eq (cst:raw (cst:second declaration)) variable)))
           ;; If it is declared special it is.
           (values t global-special-var-p))
          (global-special-var-p
           ;; It is mentioned in the environment as globally special.
           ;; if it's only special because of a local declaration,
           ;; this binding is not special.
           (values t t))
          (t
           (values nil nil)))))

;;; Given a list of canonicalized declaration specifiers for a single
;;; varible.  Return a type specifier resulting from all the type
;;; declarations present in the list.
(defun declared-type (declarations)
  `(and ,@(loop for declaration in declarations
                when (eq (cst:raw (cst:first declaration)) 'type)
                  collect (cst:raw (cst:second declaration)))))

;;; Given a single variable bound by some binding form like LET or
;;; LET*, and a list of canonical declaration specifiers
;;; concerning that variable, return a new environment that contains
;;; information about that variable.
;;;
;;; ENV is the environment to be augmented.  If the binding form has
;;; several bindings, it will contain entries for the variables
;;; preceding the one that is currently treated.
;;;
;;; ORIG-ENV is the environment in which we check whether the variable
;;; is globally special.  For a LET form, this is the environment in
;;; which the entire LET form was converted.  For a LET* form, it is
;;; the same as ENV.
(defun augment-environment-with-variable
    (variable-cst declarations system env orig-env)
  (let ((new-env env)
        (raw-variable (cst:raw variable-cst))
        (origin (cst:source variable-cst))
        (raw-declarations (mapcar #'cst:raw declarations)))
    (multiple-value-bind (special-p globally-p)
        (variable-is-special-p raw-variable declarations orig-env system)
      (if special-p
          (unless globally-p
            (setf new-env
                  (trucler:add-special-variable system new-env raw-variable)))
          (let ((lexical-variable
                  (cleavir-ast:make-lexical-variable raw-variable
                                                     :origin origin)))
            (setf new-env
                  (trucler:add-lexical-variable
                   system new-env raw-variable lexical-variable)))))
    (let* ((type (declared-type declarations))
           ;; FIXME system arguments
           (ptype (cleavir-env:parse-type-specifier type env system)))
      (unless (cleavir-ctype:top-p ptype nil)
        (setf new-env
              (trucler:add-variable-type system new-env raw-variable ptype))))
    (when (member 'ignore raw-declarations :test #'eq :key #'car)
      (setf new-env
            (trucler:add-variable-ignore system new-env raw-variable 'ignore)))
    (when (member 'ignorable raw-declarations :test #'eq :key #'car)
      (setf new-env
            (trucler:add-variable-ignore
             system new-env raw-variable 'ignorable)))
    (when (member 'dynamic-extent raw-declarations :test #'eq :key #'car)
      (setf new-env
            (trucler:add-variable-dynamic-extent system new-env raw-variable)))
    new-env))

;;; The only purpose of this function is to call the function
;;; AUGMENT-ENVIRONMENT-WITH-VARIABLE twice, once for the parameter
;;; variable and once for its associated supplied-p parameter, except
;;; that it also tests whether the supplied-p parameter is NIL,
;;; indicating that no supplied-p parameter was given.  This function
;;; returns the augmented environment.
(defun augment-environment-with-parameter (var-cst supplied-p-cst dspecs env system)
  ;; The dspecs contain declarations for both variables (and only these variables),
  ;; so we have to perform a final separation.
  (let ((new-env (augment-environment-with-variable
                  var-cst (first dspecs) system env env)))
      (if (null supplied-p-cst)
          new-env
          (augment-environment-with-variable
           supplied-p-cst (second dspecs) system new-env new-env))))

(defun augment-environment-with-local-function-name
    (name-cst environment system)
  (let* ((name (cst:raw name-cst))
         (origin (cst:source name-cst))
         (lexical-variable (cleavir-ast:make-lexical-variable
                            name :origin origin)))
    (trucler:add-local-function system environment name lexical-variable)))
