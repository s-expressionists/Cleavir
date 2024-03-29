(defpackage #:cleavir-sbcl-environment
  (:use #:common-lisp))

(require :sb-cltl2)

(in-package #:cleavir-sbcl-environment)

;;; Using the CLTL2 protocol, sbclwise.
;;; Lets you pass an sb-kernel:lexenv to cleavir. However, some of
;;; CLTL2 is broken (e.g. SBCL LP bugs #590076, #769592) so it will
;;; only work reliably with a global environment. That is, use
;;; sb-kernel:make-null-lexenv.

;;; There is an sb-int:info call in function-info. sb-int is not
;;; intended as an external interface, so this is brittle. Same for
;;; the lambda redefinitions and sb-kernel array upgrade info.
;;; It works as of SBCL 1.3.15.

(defgeneric cleavir->host (environment))

(defmethod cleavir->host (environment)
  ;; assume we're at the global
  ;; (and that the global is a host environment)
  environment)

(defmethod cleavir->host
    ((environment cleavir-env:lexical-variable))
  (sb-cltl2:augment-environment
   (cleavir->host (cleavir-env::next environment))
   :variable `(,(cleavir-env:name environment))))

(defmethod cleavir->host
    ((environment cleavir-env:special-variable))
  (sb-cltl2:augment-environment
   (cleavir->host (cleavir-env::next environment))
   :variable (list (cleavir-env:name environment))
   :declare `((cl:special ,(cleavir-env:name environment)))))

(defmethod cleavir->host ((environment cleavir-env:symbol-macro))
  (sb-cltl2:augment-environment
   (cleavir->host (cleavir-env::next environment))
   :symbol-macro `((,(cleavir-env:name environment)
                    ,(cleavir-env:expansion environment)))))

(defmethod cleavir->host ((environment cleavir-env:function))
  (sb-cltl2:augment-environment
   (cleavir->host (cleavir-env::next environment))
   :function `(,(cleavir-env:name environment))))

(defmethod cleavir->host ((environment cleavir-env:macro))
  (sb-cltl2:augment-environment
   (cleavir->host (cleavir-env::next environment))
   :macro `((,(cleavir-env:name environment)
             ,(cleavir-env:expander environment)))))

(defmethod cleavir->host ((environment cleavir-env::entry))
  (cleavir->host (cleavir-env::next environment)))

(defun cleavir-macroexpand (expander form env)
  (funcall expander form (cleavir->host env)))

(defmethod cleavir-env:eval (form environment1 (environment2 sb-kernel:lexenv))
  (sb-int:eval-in-lexenv form (cleavir->host environment1)))

(defmethod cleavir-env:variable-info
    (system (env sb-kernel:lexenv) symbol)
  (multiple-value-bind (binding local-p decls)
      (sb-cltl2:variable-information symbol env)
    (let ((dynamic-extent (cdr (assoc 'dynamic-extent decls)))
          (ignore (cdr (assoc 'ignore decls)))
          (type (cleavir-env:parse-type-specifier
                 (or (cdr (assoc 'type decls)) 't)
                 env system)))
      (ecase binding
        ((nil) nil) ; unbound
        ((:constant)
         (make-instance 'cleavir-env:constant-variable-info
                        :value (symbol-value symbol)
                        :name symbol))
        ((:special :global) ; global here is sbcl specific & hack
         (make-instance 'cleavir-env:special-variable-info
                        :global-p (not local-p)
                        :ignore ignore
                        :type type
                        :name symbol))
        ((:lexical)
         (make-instance 'cleavir-env:lexical-variable-info
                        :dynamic-extent dynamic-extent
                        :ignore ignore
                        :type type
                        :name symbol))
        ((:symbol-macro)
         (make-instance 'cleavir-env:symbol-macro-info
                        :expansion (macroexpand-1 symbol env)
                        :type type
                        :name symbol))))))

(defmethod cleavir-env:function-info (system (env sb-kernel:lexenv) symbol)
  (multiple-value-bind (binding local-p decls)
      (sb-cltl2:function-information symbol env)
    (let* ((dynamic-extent (cdr (assoc 'dynamic-extent decls)))
           (inline (cdr (assoc 'inline decls)))
           ;; only works for global functions, oh well.
           (ast (when (eq inline 'inline)
                  (let ((expansion
                          (sb-int:fun-name-inline-expansion symbol)))
                    (when expansion
                      ;; maybe cache somehow?
                      (break)
                      #+no (cleavir-generate-ast:generate-ast
                            expansion
                            ;; again, global
                            (sb-kernel:coerce-to-lexenv nil)
                            nil)))))
           (ftype (cleavir-env:parse-type-specifier
                   (or (cdr (assoc 'ftype decls)) 'function)
                   env system))
           ;; sbcl doesn't seem to actually give you this one rn
           (ignore (cdr (assoc 'ignore decls))))
      ;; SBCL defines a few special operators that also have macro definitions,
      ;; like SB-INT:CALLABLE-CAST. We just want to use the macros.
      (when (and (eq binding :special-form) (not local-p) (macro-function symbol))
        (setf binding :macro))
      (ecase binding
        ((nil) nil) ; unbound
        ((:function)
         (if local-p
             (make-instance 'cleavir-env:local-function-info
                            :dynamic-extent dynamic-extent
                            :ignore ignore
                            :inline inline
                            :type ftype
                            :name symbol)
             (make-instance 'cleavir-env:global-function-info
                            :dynamic-extent dynamic-extent
                            :ignore ignore
                            :inline inline
                            :ast ast
                            :type ftype
                            :compiler-macro
                            (compiler-macro-function symbol env)
                            :name symbol)))
        ((:macro)
         (if local-p
             (make-instance 'cleavir-env:local-macro-info
                            :expander (macro-function symbol env)
                            :name symbol)
             (make-instance 'cleavir-env:global-macro-info
                            :expander (macro-function symbol)
                            :compiler-macro
                            (compiler-macro-function symbol env)
                            :name symbol)))
        ((:special-form)
         (make-instance 'cleavir-env:special-operator-info
                        :name symbol))))))

(do-external-symbols (op :cleavir-primop)
  (unless (eql op 'cleavir-primop:call-with-variable-bound)
    (defmethod cleavir-env:function-info (system (env sb-kernel:lexenv) (sym (eql op)))
      (make-instance 'cleavir-env:special-operator-info :name sym))))

(defmethod cleavir-env:function-info (system (env sb-kernel:lexenv) (sym (eql 'cl:unwind-protect)))
  (make-instance 'cleavir-env:global-macro-info
    :compiler-macro nil
    :name 'cl:unwind-protect
    :expander (lambda (form env)
                (declare (ignore env))
                (destructuring-bind (protected &body cleanup)
                    (rest form)
                  `(%unwind-protect (lambda () ,protected) (lambda () ,@cleanup))))))

(defmethod cleavir-env:function-info (system (env sb-kernel:lexenv) (sym (eql 'cl:catch)))
  (make-instance 'cleavir-env:global-macro-info
    :compiler-macro nil
    :name 'cl:catch
    :expander (lambda (form env)
                (declare (ignore env))
                (destructuring-bind (tag &body body)
                    (rest form)
                  `(%catch ,tag (lambda () ,@body))))))

(defmethod cleavir-env:function-info (system (env sb-kernel:lexenv) (sym (eql 'cl:throw)))
  (make-instance 'cleavir-env:global-macro-info
    :compiler-macro nil
    :name 'cl:throw
    :expander (lambda (form env)
                (declare (ignore env))
                (destructuring-bind (tag result)
                    (rest form)
                  `(multiple-value-call #'%throw ,tag ,result)))))

(defmethod cleavir-env:function-info (system (env sb-kernel:lexenv) (sym (eql 'cl:progv)))
  (make-instance 'cleavir-env:global-macro-info
    :compiler-macro nil
    :name 'cl:progv
    :expander (lambda (form env)
                (declare (ignore env))
                (destructuring-bind (vars vals &body body)
                    (rest form)
                  `(%progv ,vars ,vals (lambda () ,@body))))))

(defmethod cleavir-env:function-info (system (env sb-kernel:lexenv) (sym (eql 'cl:multiple-value-call)))
  (make-instance 'cleavir-env:global-macro-info
    :compiler-macro nil
    :name 'cl:multiple-value-call
    :expander (lambda (form env)
                (declare (ignore env))
                (destructuring-bind (function-form &rest forms)
                    (rest form)
                  (let ((fsym (gensym "FUNCTION")))
                    `(let ((,fsym ,function-form))
                       (cleavir-primop:multiple-value-call
                           (etypecase ,fsym
                             (function ,fsym)
                             (symbol (fdefinition ,fsym)))
                         ,@forms)))))))

(defun %unwind-protect (protected-thunk cleanup-thunk)
  (unwind-protect (funcall protected-thunk) (funcall cleanup-thunk)))

(defun %catch (tag body-thunk)
  (catch tag (funcall body-thunk)))

(defun %throw (tag &rest values)
  (throw tag (values-list values)))

(defun %progv (vars vals body-thunk)
  (progv vars vals (funcall body-thunk)))

(defun cleavir-primop:call-with-variable-bound (variable value thunk)
  (progv (list variable) (list value)
    (funcall thunk)))

(defmethod cleavir-env:function-info (system
                                      (env sb-kernel:lexenv)
                                      (sym (eql 'sb-ext:truly-the)))
  (make-instance 'cleavir-env:special-operator-info :name 'the))

(defmethod cleavir-env:optimize-info ((env sb-kernel:lexenv))
  (let* ((decls (sb-cltl2:declaration-information 'optimize env))
         (compilation-speed
           (second (assoc 'compilation-speed decls)))
         (safety (second (assoc 'safety decls)))
         (space (second (assoc 'space decls)))
         (debug (second (assoc 'debug decls)))
         (speed (second (assoc 'speed decls)))
         (optimize `((compilation-speed ,compilation-speed)
                     (safety ,safety)
                     (space ,space)
                     (debug ,debug)
                     (speed ,speed)))
         (policy (cleavir-policy:compute-policy optimize env)))
    (make-instance 'cleavir-env:optimize-info
                   :optimize optimize
                   :policy policy)))

(defmethod cleavir-env:type-expand ((env sb-kernel:lexenv) ts)
  (sb-ext:typexpand ts env))

(defmethod cleavir-env:optimize-qualities ((env sb-kernel:lexenv))
  (loop for (opt) in (sb-cltl2:declaration-information 'optimize env)
        collect `(,opt (integer 0 3) 3)))

(defmethod cleavir-env:declarations ((env sb-kernel:lexenv))
  ;; CLTL2 only has accessors for information on a given decl, not
  ;; a list of all DECLAIM DECLARATION'd declarations.
  nil)

#+#.(cl:if (cl:find-package "CLEAVIR-KILDALL-TYPE-INFERENCE")
           '(:and)
           '(:or))
(defmethod cleavir-policy:compute-policy-quality
    ((name (eql 'cleavir-kildall-type-inference:insert-type-checks))
     optimize
     (environment sb-kernel:lexenv))
  (> (cleavir-policy:optimize-value optimize 'safety) 0))

#+#.(cl:if (cl:find-package "CLEAVIR-ESCAPE")
           '(:and)
           '(:or))
(defmethod cleavir-policy:compute-policy-quality
    ((name (eql 'cleavir-escape:trust-dynamic-extent))
     optimize
     (environment sb-kernel:lexenv))
  t)
