(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-special :before (operator cst environment system)
  (when (and *compile-time-too*
             *current-form-is-top-level-p*
             (not (member operator
                          '(progn locally macrolet symbol-macrolet eval-when))))
    (cst-eval-for-effect cst environment system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting QUOTE.

(defmethod convert-special
    ((symbol (eql 'quote)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 1)
  (cst:db s (quote-cst const-cst) cst
    (declare (ignore quote-cst))
    (convert-constant const-cst env system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting BLOCK.

(defmethod convert-special
    ((symbol (eql 'block)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (block name-cst . body-cst) cst
    (declare (ignore block))
    (let ((name (cst:raw name-cst)))
      (unless (symbolp name)
        (error 'block-name-must-be-a-symbol :cst name-cst))
      (let* ((ast (ast:make-block-ast nil :name name :origin cst))
             (new-env (env:add-block env name ast)))
        (setf (ast:body-ast ast)
              (process-progn (convert-sequence body-cst new-env system)
                             cst))
        ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting RETURN-FROM.

(defmethod convert-special
    ((symbol (eql 'return-from)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 2)
  (cst:db origin (return-from-cst block-name-cst . rest-csts) cst
    (declare (ignore return-from-cst))
    (unless (symbolp (cst:raw block-name-cst))
      (error 'block-name-must-be-a-symbol :cst block-name-cst))
    (let ((info (block-info env block-name-cst))
          (value-cst (if (cst:null rest-csts)
                         (make-atom-cst nil origin)
                         (cst:first rest-csts))))
      (ast:make-return-from-ast
       (env:identity info)
       (convert value-cst env system)
       :origin cst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting EVAL-WHEN.

(defun check-eval-when-syntax (cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (let ((situations-cst (cst:second cst)))
    (unless (cst:proper-list-p situations-cst)
      (error 'situations-must-be-proper-list :cst situations-cst))
    ;; Check each situation
    (loop for remaining = situations-cst then (cst:rest remaining)
          until (cst:null remaining)
          do (let* ((situation-cst (cst:first remaining))
                    (situation (cst:raw situation-cst)))
               (unless (and (symbolp situation)
                            (member situation
                                    '(:compile-toplevel :load-toplevel :execute
                                      compile load eval)))
                 (error 'invalid-eval-when-situation :cst situation-cst))))))

(defmethod convert-special
    ((symbol (eql 'eval-when)) cst environment system)
  (check-eval-when-syntax cst)
  (with-preserved-toplevel-ness
    (cst:db s (eval-when-cst situations-cst . body-cst) cst
      (declare (ignore eval-when-cst))
      (let* ((situations (cst:raw situations-cst))
             ;; These correspond to the abbreviations in CLHS Figure 3-7.
             (ct (or (member :compile-toplevel situations)
                     (member 'cl:compile situations)))
             (lt (or (member :load-toplevel situations)
                     (member 'cl:load situations)))
             (e  (or (member :execute situations)
                     (member 'cl:eval situations))))
        (if (or (eq *compiler* 'cl:compile)
                (eq *compiler* 'cl:eval)
                (not *current-form-is-top-level-p*))
            ;; If we're not in the file compiler, or we're not top-level,
            ;; eval-when is simple: it's progn if :execute is included, or
            ;; otherwise the whole form is discarded.
            (if e
                (process-progn
                 (convert-sequence body-cst environment system)
                 s)
                (convert (make-atom-cst nil s) environment system))
            ;; If we ARE in the file compiler, process according to Figure 3-7
            ;; in CLHS 3.2.3.1, "Processing of Top Level Forms".
            (cond (;; Process in compile-time-too mode
                   (or
                    ;; CT   LT   E    Mode
                    ;; Yes  Yes  ---  ---
                    ;; No   Yes  Yes  CTT
                    (and ct lt)
                    (and (not ct) lt e *compile-time-too*))
                   (let ((*compile-time-too* t))
                     (process-progn
                      (convert-sequence body-cst environment system))))
                  (;; Process in not-compile-time mode
                   (or
                    ;; CT   LT   E    Mode
                    ;; No   Yes  Yes  NCT
                    ;; No   Yes  No   ---
                    (and (not ct) lt e (not *compile-time-too*))
                    (and (not ct) lt (not e)))
                   (let ((*compile-time-too* nil))
                     (process-progn
                      (convert-sequence body-cst environment system))))
                  (;; Evaluate (and don't process)
                   (or
                    ;; CT   LT   E    Mode
                    ;; Yes  No   ---  ---
                    ;; No   No   Yes  CTT
                    (and ct (not lt))
                    (and (not ct) (not lt) e *compile-time-too*))
                   (cst-eval-for-effect
                    (cst:quasiquote s (progn (cst:unquote-splicing body-cst)))
                    environment system)
                   (convert (make-atom-cst nil s) environment system))
                  (;; Discard
                   ;; CT   LT    E    Mode
                   ;; No   No    Yes  NCT
                   ;; No   No    No   ---
                   ;; (But we've exhausted the cases at this point.)
                   t
                   (convert (make-atom-cst nil s) environment system))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FLET.

;;; Given a function name represented as a CST, return the name of a
;;; block (also as a CST) that should be associated with the function
;;; with that name.
(defun block-name-from-function-name (function-name)
  (if (cst:atom function-name)
      function-name
      (cst:second function-name)))

;;; Take an environment and a CST representing a single local function
;;; definition.  Return a new environment which is like the one passed
;;; as an argument, except the it has been augmented by the name of
;;; the local function.
(defun augment-environment-from-fdef (environment definition-cst)
  (let ((name-cst (cst:first definition-cst)))
    (augment-environment-with-local-function-name name-cst environment)))

;;; Take an environment, a CST representing a list of function
;;; definitions, and return a new environment which is like the one
;;; passed as an argument, except that is has been augmented by the
;;; local function names in the list.
(defun augment-environment-from-fdefs (environment definitions-cst)
  (loop with result = environment
        for remaining = definitions-cst then (cst:rest remaining)
        until (cst:null remaining)
        do (let ((definition-cst (cst:first remaining)))
             (setf result
                   (augment-environment-from-fdef result definition-cst)))
        finally (return result)))

;;; Convert a local function definition.
(defun convert-local-function (definition-cst operator environment system)
  ;; FIXME: The error message if this check fails needs improvement.
  (check-argument-count definition-cst 1 nil)
  (cst:db origin (name-cst lambda-list-cst . body-cst) definition-cst
    (unless (proper-function-name-p name-cst)
      (error 'function-name-must-be-proper-function-name
             :cst name-cst))
    (let ((block-name-cst (block-name-from-function-name name-cst)))
      (convert-code lambda-list-cst
                    body-cst
                    environment
                    system
                    :name (list operator (cst:raw name-cst))
                    :block-name-cst block-name-cst
                    :origin definition-cst))))

;;; Convert a CST representing a list of local function definitions.
(defun convert-local-functions (definitions-cst operator environment system)
  (loop for remaining = definitions-cst
          then (cst:rest remaining)
        until (cst:null remaining)
        collect (let* ((def-cst (cst:first remaining))
                       (fun (convert-local-function
                             def-cst operator environment system))
                       ;; compute these after calling convert-local-function
                       ;; so that we know def-cst is actually a list.
                       (name-cst (cst:first def-cst))
                       (name (cst:raw name-cst)))
                  (cons name fun))))

;;; Compute and return a list of LEXICAL-BIND-ASTs that will bind the
;;; AST of each function in a list of function ASTs to its associated
;;; LEXICAL-VARIABLE.  FUNCTIONS is a list of CONS cells.  Each CONS
;;; cell has a function name in its CAR and an AST in its CDR.
;;; It is known that the environment contains an entry
;;; corresponding to each function name.
(defun compute-function-init-asts (functions env system)
  (loop for (name . fun-ast) in functions
        for info = (env:function-info system env name)
        collect (ast:make-lexical-bind-ast
                 (env:identity info)
                 fun-ast
                 :origin (ast:origin fun-ast)
                 :ignore (env:ignore info)
                 )))

(defun check-function-bindings (bindings operator)
  (check-cst-proper-list bindings 'bindings-must-be-proper-list
                         :operator operator)
  (loop for remaining = bindings then (cst:rest remaining)
        until (cst:null remaining)
        do (check-cst-proper-list
            (cst:first remaining)
            'local-function-definition-must-be-proper-list)))

;;; FIXME: add the processing of DYNAMIC-EXTENT declarations.
(defmethod convert-special ((symbol (eql 'flet)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (flet-cst definitions-cst . body-cst) cst
    (declare (ignore flet-cst))
    (check-function-bindings definitions-cst 'flet)
    (multiple-value-bind (declaration-csts forms-cst)
        (cst:separate-ordinary-body body-cst)
      (let* ((canonical-declaration-specifiers
               (cst:canonicalize-declarations
                system (env:declarations env) declaration-csts))
             (defs (convert-local-functions definitions-cst symbol env system))
             (new-env (augment-environment-from-fdefs env definitions-cst))
             (final-env (augment-environment-with-declarations
                         new-env system canonical-declaration-specifiers))
             (init-asts
               (compute-function-init-asts defs final-env system)))
        (process-progn
         (append init-asts
                 (list
                  (process-progn (convert-sequence forms-cst final-env system)
                                 cst)))
         cst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LABELS.

(defmethod convert-special ((symbol (eql 'labels)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (labels-cst definitions-cst . body-cst) cst
    (declare (ignore labels-cst))
    (check-function-bindings definitions-cst 'labels)
    (multiple-value-bind (declaration-csts forms-cst)
        (cst:separate-ordinary-body body-cst)
      (let* ((canonical-declaration-specifiers
               (cst:canonicalize-declarations
                system (env:declarations env) declaration-csts))
             (new-env (augment-environment-from-fdefs env definitions-cst))
             (defs (convert-local-functions definitions-cst symbol new-env system))
             (final-env (augment-environment-with-declarations
                         new-env system canonical-declaration-specifiers))
             (init-asts
               (compute-function-init-asts defs final-env system)))
        (process-progn
         (append init-asts
                 (list
                  (process-progn
                   (convert-sequence forms-cst final-env system)
                   cst)))
         cst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting TAGBODY.
;;;
;;; The TAGBODY special form always returns NIL.  We generate a PROGN
;;; with the TAGBODY-AST and a CONSTANT-AST in it, because the
;;; TAGBODY-AST (unlike the TAGBODY special form) does not generate a
;;; value.

(defun tagp (item)
  ;; go tags are symbols or integers, per CLHS glossary.
  (or (symbolp item) (integerp item)))

;;; Returns two values. The first value is a list of CSTs for the tagbody
;;; prefix, i.e. the forms before any tags. The second value is a list of
;;; (tag-cst . form-csts) in order.
(defun parse-tagbody (body-cst)
  (loop with state = #() ; not a go tag
        with prefix = nil
        with tags = nil ; alist of (tag-ast . asts)
        with current = nil ; list of non-tag asts
        for rest = body-cst then (cst:rest rest)
        for item = (if (cst:null rest)
                       (loop-finish)
                       (cst:first rest))
        if (tagp (cst:raw item))
          do (if (vectorp state)
                 ;; we just hit the first tag,
                 ;; so everything prior is the prefix
                 (setf prefix (nreverse current))
                 ;; finish the previous tag
                 (push (cons state (nreverse current)) tags))
             (setf current nil)
             (setf state item)
        else do (push item current)
        finally (if (vectorp state)
                    ;; This tagbody had no tags
                    (setf prefix (nreverse current))
                    ;; Finish the final tag
                    (push (cons state (nreverse current)) tags))
                (return (values prefix (nreverse tags)))))

(defmethod convert-special
    ((symbol (eql 'tagbody)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (cst:db origin (tagbody-cst . body-cst) cst
    (declare (ignore tagbody-cst))
    (multiple-value-bind (prefix-csts tag-specs)
        (parse-tagbody body-cst)
      (let ((tag-asts
              (loop for (tag-cst) in tag-specs
                    collect (ast:make-tag-ast
                             (cst:raw tag-cst)
                             :origin tag-cst)))
            (new-env env))
        ;; Set up the environment for the inside of the tagbody.
        (loop for ast in tag-asts
              do (setf new-env (env:add-tag new-env (ast:name ast) ast)))
        (let ((prefix-ast
                (process-progn
                 (loop for prefix-cst in prefix-csts
                       collect (convert prefix-cst new-env system))
                 cst)))
          ;; Now compile the bodies of the tags and insert them into the
          ;; TAG-ASTs.
          (loop for tag-ast in tag-asts
                for (nil . form-csts) in tag-specs
                for seq = (loop for form-cst in form-csts
                                collect (convert form-cst new-env system))
                for progn = (process-progn seq cst)
                do (setf (ast:body-ast tag-ast) progn))
          ;; Finally, put together the tagbody, with NIL constant as described.
          (process-progn
           (list (ast:make-tagbody-ast
                  prefix-ast tag-asts :origin cst)
                 (convert-constant (make-atom-cst nil origin) env system))
           cst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting GO.

(defmethod convert-special
    ((symbol (eql 'go)) cst env system)
  (declare (ignore system))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 1)
  (cst:db origin (go-cst tag-cst) cst
    (declare (ignore go-cst))
    (let ((info (tag-info env tag-cst)))
      (ast:make-go-ast (env:identity info) :origin cst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting IF.

(defmethod convert-special ((symbol (eql 'if)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 3)
  (cst:db origin (if-cst test-cst then-cst . tail-cst) cst
    (declare (ignore if-cst))
    (let ((test-ast (convert test-cst env system))
          (true-ast (convert then-cst env system))
          (false-ast (if (cst:null tail-cst)
                         (convert-constant (make-atom-cst nil origin)
                                           env system)
                         (cst:db s (else-cst) tail-cst
                           (convert else-cst env system)))))
      (ast:make-if-ast
       test-ast
       true-ast false-ast :origin cst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOAD-TIME-VALUE.

;;; During file compilation, we generate a LOAD-TIME-VALUE-AST for a
;;; LOAD-TIME-VALUE taking a non-constant form to handle referencing
;;; the value of the form evaluated at load time in a null lexical
;;; environment. Otherwise, the semantics of load-time-value are akin
;;; to a constant reference to the value of the form evaluated at
;;; compile time in a null lexical environment.
(defmethod convert-special ((symbol (eql 'load-time-value)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 2)
  (cst:db origin (load-time-value-cst form-cst . remaining-cst) cst
    (declare (ignore load-time-value-cst))
    (let ((form (cst:raw form-cst))
          (read-only-p (if (cst:null remaining-cst)
                           nil
                           (cst:raw (cst:first remaining-cst)))))
      (unless (member read-only-p '(nil t))
        ;; The HyperSpec specifically requires a "boolean"
        ;; and not a "generalized boolean".
        (error 'read-only-p-must-be-boolean
               :cst (cst:first remaining-cst)))
      ;; FIXME: We probably want to create and use
      ;; env:constantp in case the environments don't match
      ;; up.
      (if (and (eq *compiler* 'cl:compile-file)
               (not (constantp form env)))
          (ast:make-load-time-value-ast form read-only-p :origin cst)
          (ast:make-constant-ast
           (cst-eval form-cst (env:compile-time env) system)
           :origin cst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGN.
;;;
;;; According to section 3.2.3.1 of the HyperSpec, PROGN processes
;;; its subforms the same way as the form itself.

(defmethod convert-special ((symbol (eql 'progn)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (with-preserved-toplevel-ness
    (cst:db origin (progn-cst . form-csts) cst
      (declare (ignore progn-cst))
      (process-progn (convert-sequence form-csts env system) cst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MACROLET.
;;;
;;; According to section 3.2.3.1 of the HyperSpec, MACROLET processes
;;; its subforms the same way as the form itself.

;;; Given the CST for a MACROLET definition and an environment, return
;;; a macro expander (or macro function) for the definition.
;;; FIXME: check syntax.
(defun expander (definition-cst environment system)
  (cst:db origin (name-cst lambda-list-cst . body-cst) definition-cst
    (let ((lambda-expression (cst:parse-macro system
                                              name-cst
                                              lambda-list-cst
                                              (cst:raw body-cst)
                                              environment)))
      (env:eval lambda-expression
                (env:compile-time environment)
                environment))))

(defmethod convert-special
    ((symbol (eql 'macrolet)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (macrolet-cst definitions-cst . body-cst) cst
    (declare (ignore macrolet-cst))
    (check-function-bindings definitions-cst 'macrolet)
    (let ((new-env env))
      (loop for remaining = definitions-cst then (cst:rest remaining)
            until (cst:null remaining)
            do (let* ((definition-cst (cst:first remaining))
                      (name-cst (cst:first definition-cst))
                      (name (cst:raw name-cst))
                      (expander (expander definition-cst env system)))
                 (setf new-env
                       (env:add-local-macro new-env name expander))))
      (with-preserved-toplevel-ness
        (convert (cst:quasiquote origin
                                 (locally (cst:unquote-splicing body-cst)))
                 new-env
                 system)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SYMBOL-MACROLET.

(defmethod convert-special
    ((head (eql 'symbol-macrolet)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (symbol-macrolet-cst definitions-cst . body-cst) cst
    (declare (ignore symbol-macrolet-cst))
    (let ((new-env env))
      (loop for remaining = definitions-cst then (cst:rest remaining)
            until (cst:null remaining)
            do (cst:db definition-origin (name-cst expansion-cst)
                   (cst:first remaining)
                 (let* ((name (cst:raw name-cst))
                        ;; We use cleavir-env directly, because it's
                        ;; okay if the variable is unbound.
                        (info (env:variable-info system env name))
                        (expansion (cst:raw expansion-cst)))
                   (typecase info
                     (env:constant-variable-info
                      (cerror "Bind it anyway."
                              'symbol-macro-names-constant :cst name-cst))
                     (env:special-variable-info
                      ;; Rebinding a local special is okay.
                      ;; Maybe? CLHS is a little ambiguous here - it
                      ;; says "global variable"s can't be rebound, but
                      ;; defines "global variable" to include all specials,
                      ;; possibly including local specials.
                      (when (env:global-p info)
                        (cerror "Bind it anyway."
                                'symbol-macro-names-global-special
                                :cst name-cst))))
                   (setf new-env
                         (env:add-local-symbol-macro
                          new-env name expansion)))))
      (with-preserved-toplevel-ness
        (convert (cst:quasiquote origin
                                 (locally (cst:unquote-splicing body-cst)))
                 new-env system)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FUNCTION.
;;;

(defun convert-named-function (name-cst environment system)
  (let ((info (function-info system environment name-cst)))
    (convert-function-reference name-cst info environment system)))

(defun convert-lambda-function (lambda-form-cst env system)
  (convert-code (cst:second lambda-form-cst)
                (cst:rest (cst:rest lambda-form-cst)) env system
                :origin lambda-form-cst))

(defun check-function-syntax (cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 1)
  (let ((function-name-cst (cst:second cst)))
    (cond ((proper-function-name-p function-name-cst)
           nil)
          ((cst:consp function-name-cst)
           (unless (eq (cst:raw (cst:first function-name-cst)) 'lambda)
             (error 'function-argument-must-be-function-name-or-lambda-expression
                    :cst function-name-cst))
           (unless (cst:proper-list-p function-name-cst)
             (error 'lambda-must-be-proper-list :cst function-name-cst)))
          (t
           (error 'function-argument-must-be-function-name-or-lambda-expression
                  :cst function-name-cst)))))

(defmethod convert-special ((symbol (eql 'function)) cst env system)
  (check-function-syntax cst)
  (cst:db origin (function-cst name-cst) cst
    (declare (ignore function-cst))
    (let ((result (if (proper-function-name-p name-cst)
                      (convert-named-function name-cst env system)
                      (convert-lambda-function name-cst env system))))
      (reinitialize-instance result :origin cst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THE.
;;;

(defmethod convert-special
    ((symbol (eql 'the)) cst environment system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 2)
  (cst:db origin (the-cst value-type-cst form-cst) cst
    (declare (ignore the-cst))
    (type-wrap (convert form-cst environment system)
               (env:parse-values-type-specifier
                (cst:raw value-type-cst)
                environment system)
               cst environment system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-PROG1.

(defmethod convert-special
    ((symbol (eql 'multiple-value-prog1)) cst environment system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (multiple-value-prog1-cst first-cst . rest-cst) cst
    (declare (ignore multiple-value-prog1-cst))
    (ast:make-multiple-value-prog1-ast
     (convert first-cst environment system)
     (convert-sequence rest-cst environment system)
     :origin cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods specialized to operators for which we do not provide a
;;; conversion method.

;;; Implementations should probably convert this in terms of
;;; CLEAVIR-PRIMOP:MULTIPLE-VALUE-CALL.
(defmethod convert-special
    ((symbol (eql 'multiple-value-call)) cst environment system)
  (declare (ignore environment system))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (error 'no-default-method :operator symbol :cst cst))

(defmethod convert-special
    ((symbol (eql 'unwind-protect)) cst environment system)
  (declare (ignore environment system))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (error 'no-default-method :operator symbol :cst cst))

(defmethod convert-special
    ((symbol (eql 'catch)) cst environment system)
  (declare (ignore environment system))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (error 'no-default-method :operator symbol :cst cst))

(defmethod convert-special
    ((symbol (eql 'throw)) cst environment system)
  (declare (ignore environment system))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 2)
  (error 'no-default-method :operator symbol :cst cst))

(defmethod convert-special
    ((symbol (eql 'progv)) cst environment system)
  (declare (ignore environment system))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 nil)
  (error 'no-default-method :operator symbol :cst cst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.
;;;

(defmethod convert-special
    ((symbol (eql 'setq)) cst environment system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (unless (oddp (length (cst:raw cst)))
    (error 'setq-must-have-even-number-of-arguments :cst cst))
  (let* ((csts (cst:listify (cst:rest cst)))
         (form-asts (loop for (variable-cst form-cst) on csts by #'cddr
                          for variable = (cst:raw variable-cst)
                          unless (symbolp variable)
                            do (error 'setq-var-must-be-symbol
                                      :cst variable-cst)
                          collect (convert-elementary-setq
                                   variable-cst form-cst environment system))))
    (process-progn form-asts cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET.
;;;

(defmethod convert-special
    ((symbol (eql 'let)) cst environment system)
  (convert-let cst environment system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET*.
;;;

(defmethod convert-special
    ((symbol (eql 'let*)) cst environment system)
  (convert-let* cst environment system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOCALLY.
;;;

(defmethod convert-special
    ((symbol (eql 'locally)) cst environment system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (cst:db origin (locally-cst . body-forms-cst) cst
    (declare (ignore locally-cst))
    (multiple-value-bind (declaration-csts forms-cst)
        (cst:separate-ordinary-body body-forms-cst)
      (let* ((canonical-declaration-specifiers
               (cst:canonicalize-declarations
                system (env:declarations environment) declaration-csts))
             (new-env (augment-environment-with-declarations
                       environment system canonical-declaration-specifiers)))
        (with-preserved-toplevel-ness
          (process-progn (convert-sequence forms-cst new-env system) cst))))))
