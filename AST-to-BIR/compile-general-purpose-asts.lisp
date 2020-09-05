(in-package #:cleavir-ast-to-bir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPILE-FUNCTION

(defun translate-lambda-list (lambda-list)
  (loop with ll with alist
        for item in lambda-list
        do (cond ((member item lambda-list-keywords)
                  (push item ll))
                 ((consp item)
                  (let ((valarg (make-instance 'cleavir-bir:argument
                                  :rtype :object))
                        (parg (make-instance 'cleavir-bir:argument
                                :rtype :object)))
                    (if (= (length item) 3)
                        (let ((keyv (find-or-create-variable
                                     (second item)))
                              (predv (find-or-create-variable
                                      (third item))))
                          (push (list (first item) valarg parg) ll)
                          (setf alist
                                (list* (cons keyv valarg)
                                       (cons predv parg)
                                       alist)))
                        (let ((keyv (find-or-create-variable
                                     (first item)))
                              (predv (find-or-create-variable
                                      (second item))))
                          (push (list valarg parg) ll)
                          (setf alist
                                (list* (cons keyv valarg)
                                       (cons predv parg)
                                       alist))))))
                 (t (let ((v (find-or-create-variable item))
                          (a (make-instance 'cleavir-bir:argument
                               :rtype :object)))
                      (push a ll)
                      (push (cons v a) alist))))
        finally (return (values (nreverse ll) alist))))

(defun insert-initial-bindings (inserter map)
  (loop for (var . arg) in map
        for setq = (make-instance 'cleavir-bir:writevar :variable var
                                  :inputs (list arg))
        do (before inserter setq)))

(defmethod compile-function ((ast cleavir-ast:function-ast))
  (multiple-value-bind (ll alist)
      (translate-lambda-list (cleavir-ast:lambda-list ast))
    (let* ((return (make-instance 'cleavir-bir:returni))
           (f (make-instance 'cleavir-bir:function
                :lambda-list ll
                :variables (apply #'cleavir-bir:make-set
                                  (mapcar #'car alist))))
           (end (make-instance 'cleavir-bir:iblock :dynamic-environment f))
           (inserter (make-instance 'inserter :function f)))
      (setf (cleavir-bir:end f) end)
      (reset inserter end)
      (terminate inserter return)
      (setf (cleavir-bir:inputs return)
            (list (compile-ast (cleavir-ast:body-ast ast)
                               inserter :multiple-values)))
      (let ((start (iblock inserter)))
        (insert-initial-bindings inserter alist)
        (finalize inserter)
        (setf (cleavir-bir:start f) start (cleavir-bir:inputs start) nil))
      ;; These are optional, but a lot of stuff needs them
      ;; and it's a bit less confusing to do them immediately.
      ;; Could be removed for Efficiency Reasons
      (cleavir-bir:refresh-local-iblocks f)
      (cleavir-bir:refresh-local-users f)
      f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IF-AST

(defmethod compile-ast ((ast cleavir-ast:if-ast) inserter context)
  (check-type inserter inserter)
  (assert (one-successor-context-p context))
  (let* ((iblock (iblock inserter))
         (arg (unless (effect-context-p context)
                (make-instance 'cleavir-bir:argument
                  :iblock iblock :rtype context)))
         (tjump (make-instance 'cleavir-bir:jump
                  :next (list iblock)))
         (tblock
           (make-instance 'cleavir-bir:iblock
             :dynamic-environment (cleavir-bir:dynamic-environment iblock)
             :end tjump))
         (tinserter (make-instance 'inserter
                      :function (function inserter)
                      :iblock tblock :insert-point tjump))
         (tvalue
           (compile-ast (cleavir-ast:then-ast ast)
                        tinserter context))
         (ejump (make-instance 'cleavir-bir:jump :next (list iblock)))
         (eblock
           (make-instance 'cleavir-bir:iblock
             :dynamic-environment (cleavir-bir:dynamic-environment iblock)
             :end ejump))
         (einserter (make-instance 'inserter
                      :function (function inserter)
                      :iblock eblock :insert-point ejump))
         (evalue
           (compile-ast (cleavir-ast:else-ast ast)
                        einserter context))
         (cblock
           (make-instance 'cleavir-bir:iblock
             :dynamic-environment (cleavir-bir:dynamic-environment iblock))))
    (cond
      ((effect-context-p context)
       (setf (cleavir-bir:inputs tjump) nil
             (cleavir-bir:inputs ejump) nil))
      (t
       (assert (and (cleavir-bir:rtype= tvalue context)
                    (cleavir-bir:rtype= evalue context)))
       (setf (cleavir-bir:inputs tjump) (list tvalue)
             (cleavir-bir:inputs ejump) (list evalue)
             (cleavir-bir:inputs iblock) (list arg))))
    (setf (cleavir-bir:predecessors iblock)
          (cleavir-bir:make-set tblock eblock))
    (finalize inserter)
    (reset inserter cblock)
    (compile-ast (cleavir-ast:test-ast ast)
                 inserter (list tblock eblock))
    arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BRANCH-AST

(defmethod compile-ast ((ast cleavir-ast:branch-ast) inserter context)
  (check-type inserter inserter)
  (assert (one-successor-context-p context))
  (let* ((next (iblock inserter))
         (dynenv (cleavir-bir:dynamic-environment next))
         (pre (make-instance 'cleavir-bir:iblock :dynamic-environment dynenv))
         (effectp (effect-context-p context))
         (arg (unless effectp (make-instance 'cleavir-bir:argument
                                :iblock next :rtype context)))
         (branch-asts (cleavir-ast:branch-asts ast))
         (branch-iblocks (loop repeat (length branch-asts)
                               collect (make-instance 'cleavir-bir:iblock
                                         :dynamic-environment dynenv)))
         (default-ast (cleavir-ast:default-ast ast))
         (default-iblock (make-instance 'cleavir-bir:iblock
                           :dynamic-environment dynenv))
         (jumps (loop repeat (length branch-asts)
                      collect (make-instance 'cleavir-bir:jump
                                :next (list next))))
         (default-jump (make-instance 'cleavir-bir:jump :next (list next))))
    (setf (cleavir-bir:inputs next) (list arg))
    (finalize inserter)
    (reset inserter default-iblock)
    (terminate inserter default-jump)
    (setf (cleavir-bir:inputs default-jump)
          (list (compile-ast default-ast inserter context)))
    (loop for ast in branch-asts
          for ib in branch-iblocks
          for jump in jumps
          do (reset inserter ib)
             (terminate inserter jump)
             (setf (cleavir-bir:inputs jump)
                   (list (compile-ast ast inserter context)))
             (finalize inserter))
    (reset inserter pre)
    (compile-ast (cleavir-ast:test-ast ast)
                 inserter (append branch-iblocks (list default-iblock)))
    arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PROGN-AST

(defun compile-sequence-for-effect (asts inserter)
  (loop for sub in (reverse asts)
        do (compile-ast sub inserter :effect)))

(defmethod compile-ast ((ast cleavir-ast:progn-ast) inserter context)
  (check-type inserter inserter)
  (assert (one-successor-context-p context))
  (let ((form-asts (cleavir-ast:form-asts ast)))
    (assert (not (null form-asts)))
    (let ((last (first (last form-asts)))
          (bl (butlast form-asts)))
      (prog1 (compile-ast last inserter context)
        (compile-sequence-for-effect bl inserter)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BLOCK-AST

(defun block-info (block-ast) (gethash block-ast *block-info*))
(defun (setf block-info) (new-info block-ast)
  (setf (gethash block-ast *block-info*) new-info))

(defmethod compile-ast ((ast cleavir-ast:block-ast) inserter context)
  (check-type inserter inserter)
  (assert (one-successor-context-p context))
  (let* ((next (iblock inserter))
         (contvar (make-instance 'cleavir-bir:variable :rtype :continuation))
         (function (function inserter))
         (inputs
           (if (effect-context-p context)
               nil
               (list (make-instance 'cleavir-bir:argument :rtype context))))
         (main (make-instance 'cleavir-bir:iblock))
         (pre (make-instance 'cleavir-bir:iblock
                :dynamic-environment (cleavir-bir:dynamic-environment next)))
         (catch (make-instance 'cleavir-bir:catch :next (list main next)))
         (wcont (make-instance 'cleavir-bir:writevar
                  :variable contvar :inputs (list catch)))
         (lu (make-instance 'cleavir-bir:local-unwind :next (list next))))
    (adjoin-variable inserter contvar)
    (setf (block-info ast) (list catch next function contvar context)
          (cleavir-bir:inputs next) inputs
          (cleavir-bir:dynamic-environment main) catch)
    (finalize inserter)
    (reset inserter main)
    (terminate inserter lu)
    (setf (cleavir-bir:inputs lu)
          (list (compile-ast (cleavir-ast:body-ast ast) inserter context)))
    (before inserter wcont)
    (finalize inserter)
    (reset inserter pre)
    (terminate inserter catch)
    (if (effect-context-p context)
        (values)
        (first inputs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RETURN-FROM-AST

(defmethod compile-ast ((ast cleavir-ast:return-from-ast) inserter context)
  (check-type inserter inserter)
  (let* ((block-ast (cleavir-ast:block-ast ast))
         (iblock (iblock inserter))
         (new-iblock
           (make-instance 'cleavir-bir:iblock
             :dynamic-environment (cleavir-bir:dynamic-environment iblock)))
         (function (function inserter)))
    (finalize inserter)
    (reset inserter new-iblock)
    (destructuring-bind (catch next bfunction contvar bcontext)
        (block-info block-ast)
      (if (eq function bfunction)
          ;; local
          (let ((lu (make-instance 'cleavir-bir:local-unwind)))
            (terminate inserter lu)
            (setf (cleavir-bir:inputs lu)
                  (list (compile-ast (cleavir-ast:form-ast ast)
                                     inserter bcontext))))
          ;; nonlocal
          (let ((u (make-instance 'cleavir-bir:unwind
                     :catch catch :destination next))
                (rv (make-instance 'cleavir-bir:readvar
                      :variable contvar :rtype :continuation)))
            (adjoin-variable inserter contvar)
            (cleavir-bir:nset-adjoinf (cleavir-bir:unwinds catch) u)
            (push new-iblock (cleavir-bir:entrances next))
            (terminate inserter u)
            (before inserter rv)
            (setf (cleavir-bir:inputs u)
                  (list rv
                        (compile-ast (cleavir-ast:form-ast ast)
                                     inserter bcontext)))))))
  (no-return context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TAGBODY-AST

(defun go-info (tag-ast)
  (gethash tag-ast *go-info*))

(defun (setf go-info) (new-info tag-ast)
  (setf (gethash tag-ast *go-info*) new-info))

;;; FIXME: We really oughta move this up to CST-to-AST
(defun parse-tagbody (items)
  (loop with state = #() ; not a go tag
        with prefix = nil
        with tags = nil ; alist of (tag-ast . asts)
        with current = nil ; list of non-tag asts
        for item in items
        if (typep item 'cleavir-ast:tag-ast)
          do (if (vectorp state)
                 (setf prefix (nreverse current))
                 (push (cons state (nreverse current)) tags))
             (setf state item)
        else do (push item current)
        finally (if (vectorp state)
                    (setf prefix (nreverse current))
                    (push (cons state (nreverse current)) tags))
                (return (values prefix tags))))

(defmethod compile-ast ((ast cleavir-ast:tagbody-ast) inserter context)
  (assert (effect-context-p context))
  (multiple-value-bind (prefix tags)
      (parse-tagbody (cleavir-ast:item-asts ast))
    (let* ((catch (make-instance 'cleavir-bir:catch))
           (contvar (make-instance 'cleavir-bir:variable :rtype :continuation))
           (wcont (make-instance 'cleavir-bir:writevar
                    :variable contvar :inputs (list catch)))
           (next (iblock inserter))
           (tag-iblocks
             (loop repeat (length tags)
                   collect (make-instance 'cleavir-bir:iblock
                             :dynamic-environment catch)))
           (prefix-iblock
             (make-instance 'cleavir-bir:iblock :dynamic-environment catch))
           ;; This unconditionally jumps to prefix, but it has a different
           ;; dynamic environment, so it's a different block.
           ;; They might be merged later if the tagbody is all local.
           (before
             (make-instance 'cleavir-bir:iblock
               :dynamic-environment (cleavir-bir:dynamic-environment next))))
      (adjoin-variable inserter contvar)
      ;; Set up the tag infos
      (loop for (tag-ast) in tags
            for tag-iblock in tag-iblocks
            do (setf (go-info tag-ast)
                     (list catch tag-iblock (function inserter) contvar)))
      ;; Generate code
      (flet ((gen-body (inserter body nextb iblock)
               (let ((term (if (eq nextb next)
                               (make-instance 'cleavir-bir:local-unwind
                                 :inputs nil :next (list nextb))
                               (make-instance 'cleavir-bir:jump
                                 :inputs nil :next (list nextb)))))
                 (finalize inserter)
                 (reset inserter iblock)
                 (terminate inserter term)
                 (compile-sequence-for-effect body inserter))))
        (loop with nextb = next
              for (tag-ast . body) in (reverse tags)
              for tag-iblock in (reverse tag-iblocks)
              do (gen-body inserter body nextb tag-iblock)
                 (setf nextb tag-iblock)
              finally ; generate prefix code
                      (gen-body inserter prefix nextb prefix-iblock))
        (before inserter wcont)
        (finalize inserter)
        (reset inserter before)
        (terminate inserter catch))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GO-AST

(defmethod compile-ast ((ast cleavir-ast:go-ast) inserter context)
  (finalize inserter)
  (destructuring-bind (catch next bfunction contvar)
      (go-info (cleavir-ast:tag-ast ast))
    (let ((function (function inserter)))
      (if (eq function bfunction)
          ;; local
          (before inserter (make-instance 'cleavir-bir:local-unwind
                             :inputs () :next (list next)))
          ;; nonlocal
          (let ((rv (make-instance 'cleavir-bir:readvar
                      :rtype :continuation :variable contvar)))
            (adjoin-variable inserter contvar)
            (before inserter (make-instance 'cleavir-bir:unwind
                               :inputs (list rv)
                               :catch catch :destination (list next)))
            (before inserter rv)))))
  (no-return context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALL-AST

(defun compile-arguments (arg-asts inserter)
  (loop for arg-ast in (reverse arg-asts)
        collect (compile-ast arg-ast inserter :object)))

(defmethod compile-ast ((ast cleavir-ast:call-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (one-successor-context-p context))
  (let* ((call (make-instance 'cleavir-bir:call))
         (result (figure-values inserter call context))
         (_ (before inserter call))
         (args (cleavir-ast:argument-asts ast))
         (argsvs (compile-arguments args inserter))
         (callee (cleavir-ast:callee-ast ast))
         (calleev (compile-ast callee inserter :object)))
    (declare (ignore _))
    (assert (every (lambda (a) (eq (cleavir-bir:rtype a) :object)) argsvs))
    (setf (cleavir-bir:inputs call) (list* calleev argsvs))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTION-AST

(defmethod compile-ast ((ast cleavir-ast:function-ast)
                        inserter context)
  (check-type inserter inserter)
  ;; NOTE: Could just do nothing if :effect.
  (assert (one-successor-context-p context))
  (let* ((f (or (gethash ast *function-info*)
                (setf (gethash ast *function-info*)
                      (compile-function ast))))
         (enclose (make-instance 'cleavir-bir:enclose :code f)))
    (prog1 (figure-values inserter enclose context)
      (before inserter enclose))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SETQ-AST

(defmethod compile-ast ((ast cleavir-ast:setq-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (eq context :effect))
  (let* ((var (find-or-create-variable (cleavir-ast:lhs-ast ast)))
         (assign (make-instance 'cleavir-bir:writevar :variable var)))
    (adjoin-variable inserter var)
    (before inserter assign)
    (let ((v (compile-ast (cleavir-ast:value-ast ast) inserter :object)))
      (setf (cleavir-bir:inputs assign) (list v))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; THE-AST

(defmethod compile-ast ((ast cleavir-ast:the-ast) inserter context)
  (check-type inserter inserter)
  (assert (one-successor-context-p context))
  ;;; Punt. FIXME
  (compile-ast (cleavir-ast:form-ast ast) inserter context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DYNAMIC-ALLOCATION-AST

(defmethod compile-ast ((ast cleavir-ast:dynamic-allocation-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (one-successor-context-p context))
  ;;; Punt. FIXME
  (compile-ast (cleavir-ast:form-ast ast) inserter context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO
;;; CONSTANT-SYMBOL-VALUE-AST
;;; SET-CONSTANT-SYMBOL-VALUE-AST
;;; CONSTANT-FDEFINITION-AST

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPEQ-AST

(defmethod compile-ast ((ast cleavir-ast:typeq-ast) inserter context)
  (check-type inserter inserter)
  (assert (n-next-context-p context 2))
  (let ((tq (make-instance 'cleavir-bir:typeq
              :type-specifier (cleavir-ast:type-specifier ast))))
    (terminate inserter tq)
    (setf (cleavir-bir:inputs tq)
          (list (compile-ast (cleavir-ast:form-ast ast) inserter context))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UNREACHABLE-AST

(defmethod compile-ast ((ast cleavir-ast:unreachable-ast)
                        inserter context)
  (check-type inserter inserter)
  (finalize inserter)
  (let ((next (iblock inserter)))
    (reset inserter
           (make-instance 'cleavir-bir:iblock
             :dynamic-environment (cleavir-bir:dynamic-environment next)))
    (terminate inserter (make-instance 'cleavir-bir:unreachable)))
  (no-return context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LEXICAL-AST

(defmethod compile-ast ((ast cleavir-ast:lexical-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (one-successor-context-p context))
  (let* ((var (find-or-create-variable ast))
         (rv (make-instance 'cleavir-bir:readvar :variable var)))
    (adjoin-variable inserter var)
    (prog1 (figure-values inserter rv context)
      (before inserter rv))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EQ-AST

(defmethod compile-ast ((ast cleavir-ast:eq-ast) inserter context)
  (check-type inserter inserter)
  (assert (n-next-context-p context 2))
  (let ((e (make-instance 'cleavir-bir:eqi :next (copy-list context))))
    (terminate inserter e)
    (let ((args (compile-arguments
                 (list (cleavir-ast:arg1-ast ast)
                       (cleavir-ast:arg2-ast ast))
                 inserter)))
      (setf (cleavir-bir:inputs e) args)))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NEQ-AST

(defmethod compile-ast ((ast cleavir-ast:neq-ast) inserter context)
  (check-type inserter inserter)
  (assert (n-next-context-p context 2))
  (let ((e (make-instance 'cleavir-bir:eqi :next (reverse context))))
    (terminate inserter e)
    (let ((args (compile-arguments
                 (list (cleavir-ast:arg1-ast ast)
                       (cleavir-ast:arg2-ast ast))
                 inserter)))
      (setf (cleavir-bir:inputs e) args)))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CASE-AST

(defmethod compile-ast ((ast cleavir-ast:case-ast) inserter context)
  (check-type inserter inserter)
  (let* ((arg-ast (cleavir-ast:arg-ast ast))
         (comparees (cleavir-ast:comparees ast))
         (case (make-instance 'cleavir-bir:case :comparees comparees)))
    (assert (n-next-context-p context (1+ (length comparees))))
    (terminate inserter case)
    (setf (cleavir-bir:inputs case)
          (list (compile-ast arg-ast inserter context))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LOAD-TIME-VALUE-AST. Needs work.

(defmethod compile-ast ((ast cleavir-ast:load-time-value-ast) inserter context)
  (declare (ignore inserter))
  (assert (one-successor-context-p context))
  (make-instance 'cleavir-bir:load-time-value
    :form (cleavir-ast:form ast) :read-only-p (cleavir-ast:read-only-p ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IMMEDIATE-AST. Needs work.

(defmethod compile-ast ((ast cleavir-ast:immediate-ast) inserter context)
  (declare (ignore inserter))
  (assert (one-successor-context-p context))
  (make-instance 'cleavir-bir:immediate :value (cleavir-ast:value ast)))
