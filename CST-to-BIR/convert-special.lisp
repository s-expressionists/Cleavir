(in-package #:cleavir-cst-to-bir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting QUOTE.

(defmethod convert-special
    ((symbol (eql 'quote)) cst inserter env system)
  (declare (ignore env system))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 1)
  (cst:db s (quote-cst const-cst) cst
    (declare (ignore quote-cst))
    (convert-constant const-cst inserter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting BLOCK.

(defun insert-unwind (inserter catch dest &optional inputs outputs)
  (let ((uw (make-instance 'bir:unwind
              :inputs inputs :outputs outputs :catch catch
              :destination dest)))
    (terminate inserter uw)
    (set:nadjoinf (bir:unwinds catch) uw)
    (set:nadjoinf (bir:entrances dest) (iblock inserter)))
  (values))

(defmethod convert-special
    ((symbol (eql 'block)) cst inserter env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (block name-cst . body-cst) cst
    (declare (ignore block))
    (let* ((name (cst:raw name-cst))
           (function (function inserter))
           (de (dynamic-environment inserter))
           (during (make-iblock inserter
                                :name (symbolicate '#:block- name)))
           (merge-name (symbolicate '#:block- name '#:-merge))
           (mergeb (make-iblock inserter
                                :name merge-name
                                :function function
                                :dynamic-environment de))
           (phi (make-instance 'bir:phi :iblock mergeb :name name))
           (catch (make-instance 'bir:catch
                    :next (list during mergeb) :name name))
           (block-info (list function catch mergeb))
           (new-env (env:add-block env name block-info)))
      (set:nadjoinf (bir:catches function) catch)
      (setf (bir:inputs mergeb) (list phi))
      (setf (bir:dynamic-environment during) catch)
      (terminate inserter catch)
      (begin inserter during)
      (let ((normal-rv (convert-progn body-cst inserter new-env system)))
        (unless (eq normal-rv :no-return)
          (terminate inserter
                     (make-instance 'bir:jump
                       :inputs normal-rv :outputs (list phi)
                       :next (list mergeb)))))
      (begin inserter mergeb)
      (list phi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting RETURN-FROM.

(defmethod convert-special
    ((symbol (eql 'return-from)) cst inserter env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 2)
  (cst:db origin (return-from-cst block-name-cst . rest-csts) cst
    (declare (ignore return-from-cst))
    (unless (symbolp (cst:raw block-name-cst))
      (error 'block-name-must-be-a-symbol :cst block-name-cst))
    (with-compiled-cst (rv (if (cst:null rest-csts)
                               (make-atom-cst nil origin)
                               (cst:first rest-csts))
                           inserter env system)
      (destructuring-bind (function catch mergeb)
          (env:identity (block-info env block-name-cst))
        (if (eq function (function inserter))
            (terminate inserter
                       (make-instance 'bir:jump
                         :inputs rv
                         :outputs (copy-list (bir:inputs mergeb))
                         :next (list mergeb)))
            (insert-unwind inserter catch mergeb rv
                           (copy-list (bir:inputs mergeb)))))))
  :no-return)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting EVAL-WHEN.
;;;
;;; We are not at toplevel, so we only need to worry about :execute.

(defmethod convert-special
    ((symbol (eql 'cl:eval-when)) cst inserter environment system)
  (check-eval-when-syntax cst)
  (cst:db s (eval-when-cst situations-cst . body-cst) cst
    (declare (ignore eval-when-cst))
    (let ((situations (cst:raw situations-cst)))
      (if (or (member :execute situations)
              (member 'cl:eval situations))
          (convert-progn body-cst inserter environment system)
          (convert (make-atom-cst nil s)
                   inserter environment system)))))

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
(defun augment-environment-from-fdef (environment definition-cst variable)
  (let ((name-cst (cst:first definition-cst)))
    (augment-environment-with-local-function-name name-cst
                                                  variable environment)))

;;; Take an environment, a CST representing a list of function
;;; definitions, and return a new environment which is like the one
;;; passed as an argument, except that is has been augmented by the
;;; local function names in the list.
(defun augment-environment-from-fdefs (environment definitions-cst variables)
  (loop with result = environment
        for remaining = definitions-cst then (cst:rest remaining)
        for variable in variables
        until (cst:null remaining)
        do (let ((definition-cst (cst:first remaining)))
             (setf result
                   (augment-environment-from-fdef result
                                                  definition-cst variable)))
        finally (return result)))

;;; Given an environment and the name of a function, return the
;;; VARIABLE that will have the function with that name as a
;;; value.  It is known that the environment contains an entry
;;; corresponding to the name given as an argument.
(defun function-lexical (system environment name)
  (env:identity (env:function-info system environment name)))

;;; Convert a local function definition.
(defun convert-local-function (definition-cst operator
                               inserter environment system)
  ;; FIXME: The error message if this check fails needs improvement.
  (check-argument-count definition-cst 1 nil)
  (cst:db origin (name-cst lambda-list-cst . body-cst) definition-cst
    (unless (proper-function-name-p name-cst)
      (error 'function-name-must-be-proper-function-name
             :cst name-cst))
    (let ((block-name-cst (block-name-from-function-name name-cst)))
      (convert-function lambda-list-cst
                        body-cst
                        inserter
                        environment
                        system
                        :name (list operator (cst:raw name-cst))
                        :block-name-cst block-name-cst
                        :origin origin))))

;;; Convert a CST representing a list of local function definitions.
(defun convert-local-functions (definitions-cst operator
                                inserter environment system)
  (loop for remaining = definitions-cst
          then (cst:rest remaining)
        until (cst:null remaining)
        collect (convert-local-function
                 (cst:first remaining) operator
                 inserter environment system)))

;;; Insert a sequence of LETIs that will bind the functions to their
;;; associated variables.
(defun insert-function-init-instructions (variables functions inserter)
  (loop for variable in variables
        for function in functions
        for leti = (make-instance 'bir:leti
                     :inputs function :outputs (list variable))
        do (adjoin-variable inserter variable)
           (insert inserter leti)
           (setf (bir:binder variable) leti))
  (values))

;;; Given a list CST of definitions, return a list of BIR VARIABLEs to
;;; bind them to.
(defun local-function-variables (bindings decls)
  (declare (ignore decls)) ; FIXME: propagate IGNORE
  (loop for remaining = bindings then (cst:rest remaining)
        until (cst:null remaining)
        collect (let ((name (cst:first (cst:first remaining))))
                  (make-instance 'cleavir-bir:variable
                    :name (cst:raw name)))))

;;; FIXME: add the processing of DYNAMIC-EXTENT declarations.
(defmethod convert-special ((symbol (eql 'flet)) cst inserter env system)
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
             (variables
               (local-function-variables
                definitions-cst canonical-declaration-specifiers))
             (defs (convert-local-functions definitions-cst
                                            symbol inserter env system))
             (new-env (augment-environment-from-fdefs
                       env definitions-cst variables))
             (final-env (augment-environment-with-declarations
                         new-env system canonical-declaration-specifiers)))
        (insert-function-init-instructions variables defs inserter)
        (convert-progn forms-cst inserter final-env system)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LABELS.

(defmethod convert-special ((symbol (eql 'labels)) cst inserter env system)
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
             (variables
               (local-function-variables
                definitions-cst canonical-declaration-specifiers))
             (new-env (augment-environment-from-fdefs
                       env definitions-cst variables))
             (defs (convert-local-functions
                    definitions-cst symbol inserter new-env system))
             (final-env (augment-environment-with-declarations
                         new-env system canonical-declaration-specifiers)))
        (insert-function-init-instructions variables defs inserter)
        (convert-progn forms-cst inserter final-env system)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting TAGBODY.
;;;
;;; The TAGBODY special form always returns NIL. As such we
;;; unconditionally generate a NIL constant reference after the actual
;;; tagbody code.

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
    ((symbol (eql 'tagbody)) cst inserter env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (cst:db origin (tagbody-cst . body-cst) cst
    (declare (ignore tagbody-cst))
    (multiple-value-bind (prefix-csts tag-specs)
        (parse-tagbody body-cst)
      ;; Special case: there are no tags; treat this as a progn except
      ;; the values of the last form are ignored as well.
      ;; We special case this because it's faster to do and because we
      ;; can skip making a CATCH or anything.
      (when (null tag-specs)
        (return-from convert-special
          (if (convert-list-for-effect prefix-csts inserter env system)
              (convert-constant (make-atom-cst nil system)
                                inserter env system)
              :no-return)))
      ;; General case. Create a new environment with the tags, then
      ;; compile everything in that environment, then compile a NIL
      ;; reference if the tagbody ever exits normally.
      (let* ((old-dynenv (dynamic-environment inserter))
             (function (function inserter))
             (prefix-iblock (make-iblock inserter :name '#:tagbody))
             (tags (loop for (tag-cst) in tag-specs
                         collect (cst:raw tag-cst)))
             (tag-iblocks
               (loop for tag in tags
                     ;; TAG could be an integer, so write it out
                     for tagname = (write-to-string tag)
                     for bname = (symbolicate '#:tag- tagname)
                     collecting (make-iblock inserter :name bname)))
             (catch (make-instance 'bir:catch
                      :next (list* prefix-iblock tag-iblocks)))
             (new-env env))
        ;; Set up the new environment, and while we're at it set the
        ;; dynamic environments of the new blocks.
        (setf (bir:dynamic-environment prefix-iblock) catch)
        (loop for tag in tags for ib in tag-iblocks
              for info = (list catch ib function)
              do (setf (bir:dynamic-environment ib) catch
                       new-env (env:add-tag new-env tag info)))
        ;; Generate the prefix.
        (terminate inserter catch)
        (begin inserter prefix-iblock)
        (when (convert-list-for-effect prefix-csts inserter env system)
          ;; If the prefix can continue normally, implicitly jump to the
          ;; first tag.
          (terminate inserter (make-instance 'bir:jump
                                :inputs () :outputs ()
                                :next (list (first tag-iblocks)))))
        ;; Generate the tags.
        (loop for (tag-cst . body-csts) in tag-specs
              for (ib . rest) on tag-iblocks
              do (begin inserter ib)
              if (convert-list-for-effect body-csts inserter env system)
                ;; Code continues onto the next tag, or out.
                do (let ((next
                           (if rest
                               (first rest)
                               (make-iblock inserter
                                            :name '#:tagbody-resume
                                            :dynamic-environment
                                            old-dynenv))))
                     (terminate inserter
                                (make-instance 'bir:jump
                                  :inputs () :outputs ()
                                  :next (list next)))
                     (unless rest
                       ;; Start on the block after the tagbody.
                       (begin inserter next)
                       ;; Return NIL.
                       (convert-constant (make-atom-cst nil system)
                                         inserter env system)))
                   ;; Code doesn't return.
                   ;; If this is the last tag, that means the overall
                   ;; TAGBODY doesn't return either.
              else do (unless rest
                        (return-from convert-special :no-return)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting GO.

(defmethod convert-special
    ((symbol (eql 'go)) cst inserter env system)
  (declare (ignore system))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 1)
  (cst:db origin (go-cst tag-cst) cst
    (declare (ignore go-cst))
    (destructuring-bind (catch iblock cfunction) (tag-info env tag-cst)
      (if (eq (function inserter) cfunction)
          ;; local
          (terminate inserter (make-instance 'bir:jump
                                :inputs () :outputs ()
                                :next (list iblock)))
          ;; nonlocal
          (insert-unwind inserter catch iblock))))
  :no-return)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting IF.

(defmethod convert-special ((symbol (eql 'if)) cst inserter env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 3)
  (cst:db origin (if-cst test-cst then-cst . tail-cst) cst
    (declare (ignore if-cst))
    (with-compiled-cst (test-rv test-cst inserter env system)
      (let* ((else-cst (if (cst:null tail-cst)
                           (make-atom-cst nil origin)
                           (cst:db s (else-cst) tail-cst else-cst)))
             (then-ib (make-iblock inserter :name '#:if-then))
             (else-ib (make-iblock inserter :name '#:if-else))
             (then-ins (make-instance 'inserter))
             (else-ins (make-instance 'inserter))
             (then-rv (progn
                        (proceed then-ins then-ib)
                        (convert then-cst then-ins env system)))
             (else-rv (progn
                        (proceed else-ins else-ib)
                        (convert else-cst else-ins env system))))
        (terminate inserter
                   (make-instance 'bir:ifi
                     :inputs test-rv :next (list then-ib else-ib)))
        (if (eq then-rv :no-return)
            (cond ((eq else-rv :no-return)
                   ;; no branch returns, so neither do we
                   :no-return)
                  (t ; ELSE returns, so proceed from there
                   (proceed inserter else-ib)
                   else-rv))
            (cond ((eq else-rv :no-return) ; THEN returns
                   (proceed inserter then-ib)
                   then-rv)
                  (t ; both return: create a merger block.
                   (let* ((mergeb (make-iblock inserter :name '#:if-merge))
                          (phi (make-instance 'bir:phi :iblock mergeb)))
                     ;; Jump from the branch iblocks.
                     (terminate then-ins
                                (make-instance 'bir:jump
                                  :inputs then-rv :outputs (list phi)
                                  :next (list mergeb)))
                     (terminate else-ins
                                (make-instance 'bir:jump
                                  :inputs else-rv :outputs (list phi)
                                  :next (list mergeb)))
                     ;; Start in on the merge block.
                     (setf (bir:inputs mergeb) (list phi))
                     (begin inserter mergeb)
                     (list phi)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOAD-TIME-VALUE.

;;; During file compilation, we generate a LOAD-TIME-VALUE-AST for a
;;; LOAD-TIME-VALUE taking a non-constant form to handle referencing
;;; the value of the form evaluated at load time in a null lexical
;;; environment. Otherwise, the semantics of load-time-value are akin
;;; to a constant reference to the value of the form evaluated at
;;; compile time in a null lexical environment.
(defmethod convert-special
    ((symbol (eql 'load-time-value)) cst inserter env system)
  (declare (ignore env system))
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
      (let ((ltv (bir:load-time-value-in-module form read-only-p
                                                *current-module*))
            (ltv-out (make-instance 'bir:output)))
        (insert inserter
                (make-instance 'bir:load-time-value-reference
                  :inputs (list ltv) :outputs (list ltv-out)))
        (list ltv-out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGN.
;;;

(defmethod convert-special
    ((symbol (eql 'progn)) cst inserter env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (cst:db origin (progn-cst . forms-cst) cst
    (declare (ignore progn-cst))
    (convert-progn forms-cst inserter env system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MACROLET.
;;;

(defmethod convert-special
    ((symbol (eql 'macrolet)) cst inserter env system)
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
      (convert (cst:quasiquote origin
                               (locally (cst:unquote-splicing body-cst)))
               inserter
               new-env
               system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SYMBOL-MACROLET.

(defmethod convert-special
    ((head (eql 'symbol-macrolet)) cst inserter env system)
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
      (convert (cst:quasiquote origin
                               (locally (cst:unquote-splicing body-cst)))
               inserter new-env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FUNCTION.
;;;

(defun convert-named-function (name-cst inserter environment system)
  (let ((info (function-info system environment name-cst)))
    (convert-function-reference name-cst info
                                inserter environment system)))

(defun convert-lambda-function (lambda-form-cst inserter env system)
  (convert-function (cst:second lambda-form-cst)
                    (cst:rest (cst:rest lambda-form-cst))
                    inserter env system
                    :origin (cst:source lambda-form-cst)))

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

(defmethod convert-special ((symbol (eql 'cl:function)) cst
                            inserter env system)
  (check-function-syntax cst)
  (cst:db origin (function-cst name-cst) cst
    (declare (ignore function-cst))
    (if (proper-function-name-p name-cst)
        (convert-named-function name-cst inserter env system)
        (convert-lambda-function name-cst inserter env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THE.
;;;

#+(or) ; TODO
(defmethod convert-special
    ((symbol (eql 'the)) cst inserter environment system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 2)
  (cst:db origin (the-cst value-type-cst form-cst) cst
    (declare (ignore the-cst))
    (type-wrap (convert form-cst environment system)
               (env:parse-values-type-specifier
                (cst:raw value-type-cst)
                environment system)
               origin environment system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-PROG1.

(defmethod convert-special
    ((symbol (eql 'multiple-value-prog1)) cst
     inserter environment system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (multiple-value-prog1-cst first-cst . rest-cst) cst
    (declare (ignore multiple-value-prog1-cst))
    (if (cst:null rest-cst)
        ;; Trivial case
        (convert first-cst inserter environment system)
        ;; General/hard case
        (with-compiled-cst (rv first-cst inserter environment system)
          (let* ((during (make-iblock inserter :name '#:mv-prog1-body))
                 (de (dynamic-environment inserter))
                 (save-out (make-instance 'bir:output))
                 (save (make-instance 'bir:values-save
                         :inputs rv :outputs (list save-out)
                         :next (list during))))
            (setf (bir:dynamic-environment during) save)
            (terminate inserter save)
            (begin inserter during)
            (cond ((convert-sequence-for-effect
                    rest-cst inserter environment system)
                   (let* ((read-out (make-instance 'bir:output))
                          (read (make-instance 'bir:values-collect
                                  :inputs (list save-out)
                                  :outputs (list read-out)))
                          (after (make-iblock inserter
                                              :name '#:mv-prog1-after
                                              :dynamic-environment de)))
                     (insert inserter read)
                     (terminate inserter (make-instance 'bir:jump
                                           :inputs () :outputs ()
                                           :next (list after)))
                     (begin inserter after)
                     (list read-out)))
                  (t
                   ;; The forms did not return.
                   ;; This makes our saving pointless, so hypothetically
                   ;; we could go back and remove that, but it's probably
                   ;; better to leave that to optimization passes.
                   :no-return)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods specialized to operators for which we do not provide a
;;; conversion method.

;;; Implementations should probably convert this in terms of
;;; CLEAVIR-PRIMOP:MULTIPLE-VALUE-CALL.
(defmethod convert-special
    ((symbol (eql 'multiple-value-call)) cst inserter environment system)
  (declare (ignore inserter environment system))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (error 'no-default-method :operator symbol :cst cst))

(defmethod convert-special
    ((symbol (eql 'unwind-protect)) cst inserter environment system)
  (declare (ignore inserter environment system))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (error 'no-default-method :operator symbol :cst cst))

(defmethod convert-special
    ((symbol (eql 'catch)) cst inserter environment system)
  (declare (ignore inserter environment system))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (error 'no-default-method :operator symbol :cst cst))

(defmethod convert-special
    ((symbol (eql 'throw)) cst inserter environment system)
  (declare (ignore inserter environment system))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 2)
  (error 'no-default-method :operator symbol :cst cst))

(defmethod convert-special
    ((symbol (eql 'progv)) cst inserter environment system)
  (declare (ignore inserter environment system))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 nil)
  (error 'no-default-method :operator symbol :cst cst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.
;;;

(defmethod convert-special
    ((symbol (eql 'setq)) cst inserter environment system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (unless (oddp (length (cst:raw cst)))
    (error 'setq-must-have-even-number-of-arguments :cst cst))
  (loop for remaining = (cst:rest cst)
          then (cst:rest (cst:rest remaining))
        until (cst:null remaining)
        do (let ((variable-cst (cst:first remaining))
                 (form-cst (cst:first (cst:rest remaining))))
             (unless (symbolp (cst:raw variable-cst))
               (error 'setq-var-must-be-symbol variable-cst))
             (convert-elementary-setq variable-cst form-cst
                                      inserter environment system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET.
;;;

(defmethod convert-special
    ((symbol (eql 'let)) cst inserter environment system)
  (convert-let cst inserter environment system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET*.
;;;

(defmethod convert-special
    ((symbol (eql 'let*)) cst inserter environment system)
  (convert-let* cst inserter environment system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOCALLY.
;;;

(defmethod convert-special
    ((symbol (eql 'locally)) cst inserter environment system)
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
        (convert-progn forms-cst inserter new-env system)))))
