(in-package #:cleavir-ast-to-bir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPILE-FUNCTION

(defun translate-lambda-list (lambda-list funct)
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
                                     (second item)
                                     funct))
                              (predv (find-or-create-variable
                                      (third item)
                                      funct)))
                          (push (list (first item) valarg parg) ll)
                          (setf alist
                                (list* (cons keyv valarg)
                                       (cons predv parg)
                                       alist)))
                        (let ((keyv (find-or-create-variable
                                     (first item)
                                     funct))
                              (predv (find-or-create-variable
                                      (second item)
                                      funct)))
                          (push (list valarg parg) ll)
                          (setf alist
                                (list* (cons keyv valarg)
                                       (cons predv parg)
                                       alist))))))
                 (t (let ((v (find-or-create-variable item funct))
                          (a (make-instance 'cleavir-bir:argument
                               :rtype :object)))
                      (push a ll)
                      (push (cons v a) alist))))
        finally (return (values (nreverse ll) alist))))

(defun insert-initial-bindings (inserter map)
  (loop for (var . arg) in map
        for setq = (make-instance 'cleavir-bir:writevar
                     :outputs (list var) :inputs (list arg))
        do (before inserter setq)))

(defmethod compile-function ((ast cleavir-ast:function-ast))
  (let* ((return (make-instance 'cleavir-bir:returni))
         (f (make-instance 'cleavir-bir:function ))
         (end (make-instance 'cleavir-bir:iblock
                :function f :dynamic-environment f))
         (inserter (make-instance 'inserter)))
    (multiple-value-bind (ll alist)
        (translate-lambda-list (cleavir-ast:lambda-list ast) f)
      (setf (cleavir-bir:lambda-list f) ll
            (cleavir-bir:variables f) (apply #'cleavir-set:make-set
                                             (mapcar #'car alist)))
      (setf (cleavir-bir:end f) end)
      (reset inserter end)
      (terminate inserter return)
      (setf (cleavir-bir:inputs return)
            (compile-ast (cleavir-ast:body-ast ast)
                         inserter :multiple-values))
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

(defun compile-branch (inserter test-ast branch-asts context)
  (check-type inserter inserter)
  (assert (context-p context))
  (let* ((next (iblock inserter))
         (dynenv (cleavir-bir:dynamic-environment next))
         (pre (make-instance 'cleavir-bir:iblock
                :function (function inserter) :dynamic-environment dynenv))
         (effectp (effect-context-p context))
         (phis (cond (effectp nil)
                     ((eq context :multiple-values)
                      (list (make-instance 'cleavir-bir:phi
                              :iblock next :rtype :multiple-values)))
                     (t (loop for rtype in context
                              collect (make-instance 'cleavir-bir:phi
                                        :iblock next :rtype rtype)))))
         (branch-iblocks (loop repeat (length branch-asts)
                               collect (make-instance 'cleavir-bir:iblock
                                         :function (function inserter)
                                         :inputs ()
                                         :dynamic-environment dynenv)))
         (jumps (loop repeat (length branch-asts)
                      collect (make-instance 'cleavir-bir:jump
                                :outputs (copy-list phis) :next (list next)))))
    (setf (cleavir-bir:inputs next) phis)
    (finalize inserter)
    (loop for ast in branch-asts
          for ib in branch-iblocks
          for jump in jumps
          do (reset inserter ib)
             (terminate inserter jump)
             (setf (cleavir-bir:inputs jump)
                   (compile-ast ast inserter context))
             (finalize inserter))
    (reset inserter pre)
    (compile-test-ast test-ast inserter branch-iblocks)
    phis))

(defmethod compile-ast ((ast cleavir-ast:if-ast) inserter context)
  (compile-branch inserter (cleavir-ast:test-ast ast)
                  (list (cleavir-ast:then-ast ast) (cleavir-ast:else-ast ast))
                  context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BRANCH-AST

(defmethod compile-ast ((ast cleavir-ast:branch-ast) inserter context)
  (compile-branch inserter (cleavir-ast:test-ast ast)
                  (append (cleavir-ast:branch-asts ast)
                          (list (cleavir-ast:default-ast ast)))
                  context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PROGN-AST

(defun compile-sequence-for-effect (asts inserter)
  (loop for sub in (reverse asts)
        do (compile-ast sub inserter :effect)))

(defmethod compile-ast ((ast cleavir-ast:progn-ast) inserter context)
  (check-type inserter inserter)
  (assert (context-p context))
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
  (assert (context-p context))
  (let* ((next (iblock inserter))
         (nde (cleavir-bir:dynamic-environment next))
         (function (function inserter))
         (phis (case context
                 (:effect nil)
                 (:multiple-values
                  (list (make-instance 'cleavir-bir:phi
                          :iblock next :rtype :multiple-values)))
                 (t (loop for rtype in context
                          collect (make-instance 'cleavir-bir:phi
                                    :iblock next :rtype rtype)))))
         (main (make-instance 'cleavir-bir:iblock :function function))
         (pre (make-instance 'cleavir-bir:iblock
                :function function :dynamic-environment nde))
         (catch (make-instance 'cleavir-bir:catch
                  :next (list next)))
         (contvar (make-instance 'cleavir-bir:variable
                    :binder catch :rtype :continuation))
         (wcont (make-instance 'cleavir-bir:writevar
                  :outputs (list contvar) :inputs (list catch)))
         (lu (make-instance 'cleavir-bir:jump
               :unwindp t :outputs phis :next (list next))))
    (adjoin-variable inserter contvar)
    (setf (block-info ast) (list catch next function contvar context)
          (cleavir-bir:inputs next) phis
          (cleavir-bir:dynamic-environment main) catch)
    (finalize inserter)
    (reset inserter main)
    (terminate inserter lu)
    (setf (cleavir-bir:inputs lu)
          (compile-ast (cleavir-ast:body-ast ast) inserter context))
    (before inserter wcont)
    (push (iblock inserter) (cleavir-bir:next catch))
    (finalize inserter)
    (reset inserter pre)
    (terminate inserter catch)
    phis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RETURN-FROM-AST

(defmethod compile-ast ((ast cleavir-ast:return-from-ast) inserter context)
  (check-type inserter inserter)
  (let* ((block-ast (cleavir-ast:block-ast ast))
         (iblock (iblock inserter))
         (new-iblock
           (make-instance 'cleavir-bir:iblock
             :function (function inserter)
             :dynamic-environment (cleavir-bir:dynamic-environment iblock)))
         (function (function inserter)))
    (finalize inserter)
    (reset inserter new-iblock)
    (destructuring-bind (catch next bfunction contvar bcontext)
        (block-info block-ast)
      (if (eq function bfunction)
          ;; local
          (let ((lu (make-instance 'cleavir-bir:jump
                      :next (list next) :outputs (cleavir-bir:inputs next)
                      :unwindp t)))
            (terminate inserter lu)
            (let ((compiled
                    (compile-ast (cleavir-ast:form-ast ast)
                                 inserter bcontext)))
              (setf (cleavir-bir:inputs lu)
                    (case bcontext
                      (:effect)
                      (:multiple-values (list compiled))
                      (t compiled)))))
          ;; nonlocal
          (let ((u (make-instance 'cleavir-bir:unwind
                     :catch catch :destination next
                     :outputs (cleavir-bir:inputs next)))
                (rv (make-instance 'cleavir-bir:readvar
                      :inputs (list contvar) :rtype :continuation)))
            (adjoin-variable inserter contvar)
            (cleavir-set:nadjoinf (cleavir-bir:unwinds catch) u)
            (cleavir-set:nadjoinf (cleavir-bir:entrances next) new-iblock)
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
    (let* ((next (iblock inserter))
           (nde (cleavir-bir:dynamic-environment next))
           (tag-iblocks
             (loop repeat (length tags)
                   collect (make-instance 'cleavir-bir:iblock
                             :function (function inserter))))
           (prefix-iblock
             (make-instance 'cleavir-bir:iblock :function (function inserter)))
           (catch (make-instance 'cleavir-bir:catch
                    :next (list* prefix-iblock tag-iblocks)))
           (contvar (make-instance 'cleavir-bir:variable
                      :binder catch :rtype :continuation))
           (wcont (make-instance 'cleavir-bir:writevar
                    :outputs (list contvar) :inputs (list catch)))
           ;; This unconditionally jumps to prefix, but it has a different
           ;; dynamic environment, so it's a different block.
           ;; They might be merged later if the tagbody is all local.
           (before
             (make-instance 'cleavir-bir:iblock
               :function (function inserter) :dynamic-environment nde)))
      (adjoin-variable inserter contvar)
      ;; Set up the tag infos and dynamic environments
      (setf (cleavir-bir:dynamic-environment prefix-iblock) catch)
      (loop for (tag-ast) in tags
            for tag-iblock in tag-iblocks
            do (setf (cleavir-bir:dynamic-environment tag-iblock) catch)
               (setf (go-info tag-ast)
                     (list catch tag-iblock (function inserter) contvar)))
      ;; Generate code
      (flet ((gen-body (inserter body nextb iblock)
               (let ((term (make-instance 'cleavir-bir:jump
                             :inputs nil :next (list nextb)
                             :unwindp (eq nextb next))))
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
          (before inserter (make-instance 'cleavir-bir:jump
                             :unwindp t :inputs () :outputs ()
                             :next (list next)))
          ;; nonlocal
          (let ((rv (make-instance 'cleavir-bir:readvar
                      :rtype :continuation :inputs (list contvar))))
            (adjoin-variable inserter contvar)
            (before inserter (make-instance 'cleavir-bir:unwind
                               :inputs (list rv) :outputs ()
                               :catch catch :destination (list next)))
            (before inserter rv)))))
  (no-return context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALL-AST

(defun compile-arguments (arg-asts inserter)
  (nreverse
   (loop for arg-ast in (reverse arg-asts)
         collect (first (compile-ast arg-ast inserter '(:object))))))

(defmethod compile-ast ((ast cleavir-ast:call-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (context-p context))
  (let* ((call (make-instance 'cleavir-bir:call))
         (result (figure-mvalues inserter call context))
         (_ (before inserter call))
         (args (cleavir-ast:argument-asts ast))
         (argsvs (compile-arguments args inserter))
         (callee (cleavir-ast:callee-ast ast))
         (calleev (first (compile-ast callee inserter '(:object)))))
    (declare (ignore _))
    (assert (every (lambda (a) (eq (cleavir-bir:rtype a) :object)) argsvs))
    (setf (cleavir-bir:inputs call) (list* calleev argsvs))
    (list result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTION-AST

(defmethod compile-ast ((ast cleavir-ast:function-ast)
                        inserter context)
  (check-type inserter inserter)
  ;; NOTE: Could just do nothing if :effect.
  (assert (context-p context))
  (let* ((f (or (gethash ast *function-info*)
                (setf (gethash ast *function-info*)
                      (compile-function ast))))
         (enclose (make-instance 'cleavir-bir:enclose :code f)))
    (cleavir-set:nadjoinf (cleavir-bir:encloses f) enclose)
    (return-1 inserter enclose context)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SETQ-AST

(defmethod compile-ast ((ast cleavir-ast:setq-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (eq context :effect))
  (let* ((var (find-or-create-variable (cleavir-ast:lhs-ast ast)
                                       (function inserter)))
         (assign (make-instance 'cleavir-bir:writevar :outputs (list var))))
    (adjoin-variable inserter var)
    (before inserter assign)
    (let ((v (compile-ast (cleavir-ast:value-ast ast) inserter '(:object))))
      (setf (cleavir-bir:inputs assign) v)))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; THE-AST

(defmethod compile-ast ((ast cleavir-ast:the-ast) inserter context)
  (check-type inserter inserter)
  (assert (context-p context))
  ;;; Punt. FIXME
  (compile-ast (cleavir-ast:form-ast ast) inserter context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DYNAMIC-ALLOCATION-AST

(defmethod compile-ast ((ast cleavir-ast:dynamic-allocation-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (context-p context))
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

(defmethod compile-test-ast ((ast cleavir-ast:typeq-ast) inserter successors)
  (check-type inserter inserter)
  (assert (= (length successors) 2))
  (let ((tq (make-instance 'cleavir-bir:typeq
              :next (copy-list successors)
              :type-specifier (cleavir-ast:type-specifier ast))))
    (terminate inserter tq)
    (setf (cleavir-bir:inputs tq)
          (list (compile-ast (cleavir-ast:form-ast ast) inserter '(:object)))))
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
             :function (function inserter)
             :dynamic-environment (cleavir-bir:dynamic-environment next)))
    (terminate inserter (make-instance 'cleavir-bir:unreachable)))
  (no-return context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LEXICAL-AST

(defmethod compile-ast ((ast cleavir-ast:lexical-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (context-p context))
  (let* ((var (find-or-create-variable ast (function inserter)))
         (rv (make-instance 'cleavir-bir:readvar :inputs (list var))))
    (adjoin-variable inserter var)
    (return-1 inserter rv context)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EQ-AST

(defmethod compile-test-ast ((ast cleavir-ast:eq-ast) inserter successors)
  (check-type inserter inserter)
  (assert (= (length successors) 2))
  (let ((e (make-instance 'cleavir-bir:eqi :next (copy-list successors))))
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

(defmethod compile-test-ast ((ast cleavir-ast:neq-ast) inserter successors)
  (check-type inserter inserter)
  (assert (= (length successors) 2))
  (let ((e (make-instance 'cleavir-bir:eqi :next (reverse successors))))
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

(defmethod compile-test-ast ((ast cleavir-ast:case-ast) inserter successors)
  (check-type inserter inserter)
  (let* ((arg-ast (cleavir-ast:arg-ast ast))
         (comparees (cleavir-ast:comparees ast))
         (case (make-instance 'cleavir-bir:case
                 :comparees comparees :next (copy-list successors))))
    (assert (= (length successors) (1+ (length comparees))))
    (terminate inserter case)
    (setf (cleavir-bir:inputs case)
          (list (compile-ast arg-ast inserter '(:object)))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LOAD-TIME-VALUE-AST. Needs work.

(defmethod compile-ast ((ast cleavir-ast:load-time-value-ast) inserter context)
  (assert (context-p context))
  (return-1
   inserter
   (make-instance 'cleavir-bir:load-time-value
     :form (cleavir-ast:form ast) :read-only-p (cleavir-ast:read-only-p ast))
   context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IMMEDIATE-AST. Needs work.

(defmethod compile-ast ((ast cleavir-ast:immediate-ast) inserter context)
  (assert (context-p context))
  (return-1
   inserter
   (make-instance 'cleavir-bir:immediate :value (cleavir-ast:value ast))
   context))
