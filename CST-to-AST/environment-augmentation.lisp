(cl:in-package #:cleavir-cst-to-ast)

;;; Augment the environment with a single canonicalized declaration
;;; specifier.
(defgeneric augment-environment-with-declaration
    (client
     declaration-identifier
     declaration-identifier-cst
     declaration-data-cst
     environment))

(defmethod augment-environment-with-declaration
    (client
     declaration-identifier
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (declare (ignore client
                   declaration-identifier-cst
                   declaration-data-cst))
  (warn "Unable to handle declarations specifier: ~s"
        declaration-identifier)
  environment)

(defmethod augment-environment-with-declaration
    (client
     (declaration-identifier (eql 'dynamic-extent))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (declare (ignore declaration-identifier-cst))
  (let ((var-or-function (cst:raw (cst:first declaration-data-cst))))
    (if (consp var-or-function)
        ;; (dynamic-extent (function foo))
        (trucler:add-function-dynamic-extent
         client environment (second var-or-function))
        ;; (dynamic-extent foo)
        (trucler:add-variable-dynamic-extent
         client environment var-or-function))))

(defmethod augment-environment-with-declaration
    (client
     (declaration-identifier (eql 'ftype))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (declare (ignore declaration-identifier-cst))
  (trucler:add-function-type
   client environment (cst:raw (cst:second declaration-data-cst))
   (parse-type-specifier
    client
    (cst:raw (cst:first declaration-data-cst))
    environment)))

(defmethod augment-environment-with-declaration
    (client
     (declaration-identifier (eql 'ignore))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (let ((var-or-function (cst:raw (cst:first declaration-data-cst)))
        (ignore (cst:raw declaration-identifier-cst)))
    (if (consp var-or-function)
        (trucler:add-function-ignore
         client environment (second var-or-function) ignore)
        (trucler:add-variable-ignore
         client environment var-or-function ignore))))

(defmethod augment-environment-with-declaration
    (client
     (declaration-identifier (eql 'ignorable))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (let ((var-or-function (cst:raw (cst:first declaration-data-cst)))
        (ignore (cst:raw declaration-identifier-cst)))
    (if (consp var-or-function)
        (trucler:add-function-ignore
         client environment (second var-or-function) ignore)
        (trucler:add-variable-ignore
         client environment var-or-function ignore))))

(defmethod augment-environment-with-declaration
    (client
     (declaration-identifier (eql 'inline))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (trucler:add-inline
   client environment (cst:raw (cst:first declaration-data-cst))
   (cst:raw declaration-identifier-cst)))

(defmethod augment-environment-with-declaration
    (client
     (declaration-identifier (eql 'notinline))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (trucler:add-inline
   client environment (cst:raw (cst:first declaration-data-cst))
   (cst:raw declaration-identifier-cst)))

(defmethod augment-environment-with-declaration
    (client
     (declaration-identifier (eql 'special))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (declare (ignore declaration-identifier-cst))
  ;; This case is a bit tricky, because if the
  ;; variable is globally special, nothing should
  ;; be added to the environment.
  (let ((description (trucler:describe-variable
                      client environment
                      (cst:raw (cst:first declaration-data-cst)))))
    (typecase description
      (trucler:symbol-macro-description
       (error 'special-symbol-macro
              :cst (cst:first declaration-data-cst)))
      (trucler:global-special-variable-description
       environment)
      (t (trucler:add-local-special-variable
          client environment
          (cst:raw (cst:first declaration-data-cst)))))))

(defmethod augment-environment-with-declaration
    (client
     (declaration-identifier (eql 'type))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (declare (ignore declaration-identifier-cst))
  (cst:db source (type-cst variable-cst) declaration-data-cst
    (trucler:add-variable-type
     client environment (cst:raw variable-cst)
     (parse-type-specifier client (cst:raw type-cst) environment))))

(defmethod augment-environment-with-declaration
    (client
     (declaration-identifier (eql 'optimize))
     declaration-identifier-cst
     declaration-data-cst
     environment)
  (declare (ignore client
                   declaration-identifier-cst
                   declaration-data-cst))
  ;; OPTIMIZE is handled specially, so we do nothing here.
  ;; This method is just for ensuring that the default method,
  ;; which signals a warning, isn't called.
  environment)

(defun augment-environment-with-single-optimize (client optimize environment)
  (multiple-value-bind (quality value)
      (if (symbolp optimize)
          (values optimize 3)
          (values (first optimize) (second optimize)))
    (ecase quality
      (speed (trucler:add-speed client environment value))
      (compilation-speed (trucler:add-compilation-speed client environment value))
      (debug (trucler:add-debug client environment value))
      (space (trucler:add-space client environment value))
      (safety (trucler:add-safety client environment value)))))

;;; Augment the environment with an OPTIMIZE specifier.
(defun augment-environment-with-optimize (client optimize environment)
  (let ((result environment))
    (dolist (single-optimize optimize result)
      (setf result (augment-environment-with-single-optimize
                    client single-optimize result)))))

;;; Extract any OPTIMIZE information from a set of canonicalized
;;; declaration specifiers.
(defun extract-optimize (canonicalized-dspecs)
  (loop for spec in canonicalized-dspecs
        when (eq (cst:raw (cst:first spec)) 'optimize)
          append (mapcar #'cst:raw (cst:listify (cst:rest spec)))))

;;; Augment the environment with a list of canonical declartion
;;; specifiers.
(defun augment-environment-with-declarations (client environment
                                              canonical-dspecs)
  (let ((new-env
          ;; handle OPTIMIZE specially.
          (let ((optimize (extract-optimize canonical-dspecs)))
            (if optimize
                (augment-environment-with-optimize client optimize environment)
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
                    client
                    declaration-identifier
                    declaration-identifier-cst
                    declaration-data-cst
                    new-env)))
    new-env))

;;; Given a single variable bound by some binding form, a list of
;;; canonicalized declaration specifiers, and an environment in which
;;; the binding form is compiled, return true if and only if the
;;; variable to be bound is special.  Return a second value indicating
;;; whether the variable is globally special.
(defun variable-is-special-p (variable declarations existing-var-description)
  (let ((special-var-p
          (typep existing-var-description 'trucler:special-variable-description)))
    (cond ((loop for declaration in declarations
                 thereis (and (eq (cst:raw (cst:first declaration)) 'special)
                              (eq (cst:raw (cst:second declaration)) variable)))
           ;; If it is declared special it is.
           (values t (and special-var-p
                          (typep existing-var-description
                                 'trucler:global-special-variable-description))))

          ((and special-var-p
                (typep existing-var-description
                       'trucler:global-special-variable-description))
           ;; It is mentioned in the environment as globally special.
           (values t t))
          (t
           ;; If it's only special because of a local declaration,
           ;; this binding is not special.
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
    (client variable-cst declarations env orig-env)
  (let* ((new-env env)
         (raw-variable (cst:raw variable-cst))
         (raw-declarations (mapcar #'cst:raw declarations))
         (description (trucler:describe-variable client orig-env raw-variable)))
    (when (typep description 'trucler:constant-variable-description)
      (warn 'bind-constant-variable :cst variable-cst))
    (multiple-value-bind (special-p globally-p)
        (variable-is-special-p raw-variable declarations description)
      (if special-p
          (unless globally-p
            (setf new-env
                  (trucler:add-local-special-variable
                   client new-env raw-variable)))
          (let ((lexical-variable (ast:make-lexical-variable
                                   raw-variable :origin variable-cst)))
            (setf new-env
                  (trucler:add-lexical-variable
                   client new-env raw-variable lexical-variable)))))
    (let* ((type (declared-type declarations))
           (ptype (parse-type-specifier client type env)))
      (unless (ctype:top-p client ptype)
        (setf new-env
              (trucler:add-variable-type
               client new-env raw-variable ptype))))
    (when (member 'ignore raw-declarations :test #'eq :key #'car)
      (setf new-env
            (trucler:add-variable-ignore
             client new-env raw-variable 'ignore)))
    (when (member 'ignorable raw-declarations :test #'eq :key #'car)
      (setf new-env
            (trucler:add-variable-ignore
             client new-env raw-variable 'ignorable)))
    (when (member 'dynamic-extent raw-declarations :test #'eq :key #'car)
      (setf new-env
            (trucler:add-variable-dynamic-extent
             client new-env raw-variable)))
    new-env))

;;; The only purpose of this function is to call the function
;;; AUGMENT-ENVIRONMENT-WITH-VARIABLE twice, once for the parameter
;;; variable and once for its associated supplied-p parameter, except
;;; that it also tests whether the supplied-p parameter is NIL,
;;; indicating that no supplied-p parameter was given.  This function
;;; returns the augmented environment.
(defun augment-environment-with-parameter (client var-cst supplied-p-cst dspecs env)
  ;; The dspecs contain declarations for both variables (and only these variables),
  ;; so we have to perform a final separation.
  (let ((new-env (augment-environment-with-variable
                  client var-cst (first dspecs) env env)))
      (if (null supplied-p-cst)
          new-env
          (augment-environment-with-variable
           client supplied-p-cst (second dspecs) new-env new-env))))

(defun augment-environment-with-local-function-name (client name-cst environment)
  (let* ((name (cst:raw name-cst))
         (lexical-variable (ast:make-lexical-variable name :origin name-cst)))
    (trucler:add-local-function client environment name lexical-variable)))
