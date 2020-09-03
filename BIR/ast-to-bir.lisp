(in-package #:cleavir-bir)

(defvar *variables*)
(defvar *block-info*)
(defvar *go-info*)
(defvar *function-info*)

(defun find-or-create-variable (lexical-ast)
  (check-type lexical-ast cleavir-ast:lexical-ast)
  (or (gethash lexical-ast *variables*)
      (setf (gethash lexical-ast *variables*)
            (make-instance 'variable :rtype :object))))

(defclass inserter ()
  ((%iblock :initarg :iblock :accessor iblock)
   (%insert-point :initarg :insert-point :accessor insert-point
                  :type instruction)
   (%function :initarg :function :reader function)))

(defun add-iblock-to-function (iblock function)
  (setf (iblocks function)
        (nset-adjoin iblock (iblocks function))))

(defun before (inserter instruction)
  (check-type inserter inserter)
  (check-type instruction instruction)
  (let ((ip (insert-point inserter)))
    (check-type ip instruction)
    (assert (null (predecessor ip)))
    (setf (predecessor ip) instruction
          (successor instruction) ip
          (insert-point inserter) instruction))
  instruction)

(defun finalize (inserter)
  (check-type inserter inserter)
  (setf (start (iblock inserter)) (insert-point inserter))
  (slot-makunbound inserter '%insert-point))

(defun reset (inserter iblock)
  (setf (iblock inserter) iblock)
  (add-iblock-to-function iblock (function inserter)))

(defun terminate (inserter terminator)
  (check-type inserter inserter)
  (check-type terminator terminator)
  (let ((i (iblock inserter)))
    (loop for next in (next terminator)
          do (setf (predecessors next)
                   (nset-adjoin i (predecessors next))))
    (setf (start i) terminator
          (end i) terminator
          (insert-point inserter) terminator)))

(defun object-aggregate-p (rtype)
  (and (aggregatep rtype)
       (loop for i below (aggregate-length rtype)
             always (rtype= (aggregate-elt rtype i)
                            :object))))

(defun primary-value (inserter value)
  (let ((rt (rtype value)))
    (case rt
      (:object value)
      (:multiple-values
       (let* ((mtf (make-instance 'multiple-to-fixed
                     :inputs (list value)
                     :rtype '(:object)))
              (ext (make-instance 'extract
                     :index 0
                     :inputs (list mtf))))
         (before inserter ext)
         (before inserter mtf)
         ext))
      (t
       (assert (object-aggregate-p rt))
       (if (zerop (aggregate-length rt))
           (make-constant nil)
           (before inserter
                   (make-instance 'extract :index 0 :inputs (list value))))))))

(defun multiple-values (inserter value)
  (let ((rt (rtype value)))
    (case rt
      (:multiple-values value)
      (:object
       (let* ((create (make-instance 'create
                        :inputs (list value)
                        :rtype '#.(aggregate :object)))
              (ftm (make-instance 'fixed-to-multiple
                     :inputs (list create))))
         (before inserter ftm)
         (before inserter create)
         ftm))
      (t
       (assert (object-aggregate-p rt))
       (before inserter (make-instance 'fixed-to-multiple
                          :inputs (list value)))))))

(defun to-object-aggregate (inserter value agg)
  (let ((rt (rtype value)))
    (case rt
      (:multiple-values
       (before inserter (make-instance 'multiple-to-fixed
                          :inputs (list value)
                          :rtype agg)))
      (:object
       (before inserter (make-instance 'create
                          :inputs (list value)
                          :rtype agg)))
      (t
       (assert (object-aggregate-p agg))
       (let* ((target-len (aggregate-length agg))
              (source-len (aggregate-length rt)))
         (if (= target-len source-len)
             value
             (let*
                 ((shared-len (min target-len source-len))
                  (extracts
                    (loop for i below shared-len
                          collect (make-instance 'extract
                                    :index i :inputs (list value))))
                  (nils
                    (loop for i from shared-len below target-len
                          collect (make-constant nil)))
                  (create
                    (make-instance 'create
                      :rtype agg :inputs (append extracts nils))))
               (before inserter create)
               (loop for e in extracts do (before inserter create))
               create)))))))

(defun figure-values (inserter value context)
  (case context
    (:multiple-values (multiple-values inserter value))
    (:object (primary-value inserter value))
    (:effect (values))
    (t
     (assert (object-aggregate-p context))
     (to-object-aggregate inserter value context))))

;;; This is used internally in ast-to-hir for when a form
;;; that was compiled in a value context never returns, e.g. due to an
;;; unreachable or return-from.
(defclass dummy (value) ())

(defun no-return (context)
  (if (effect-context-p context)
      (values)
      (make-instance 'dummy)))

;;; A context is either:
;;; an rtype, indicating a value is to be returned, or
;;; a list of blocks, indicating a branch is expected, or
;;; :effect, indicating any value computed will be discarded

;;; A side-effects-only context; value discarded
(defun effect-context-p (context)
  (eq context :effect))

(defun one-successor-context-p (context)
  (or (effect-context-p context)
      (typep context 'rtype)))

(defun n-next-context-p (context n)
  (and (listp context) (eql (length context) n)))

(defgeneric compile-function (ast))

(defun compile-toplevel (ast)
  (let ((*variables* (make-hash-table :test #'eq))
        (*block-info* nil)
        (*go-info* nil)
        (*function-info* (make-hash-table :test #'eq)))
    (compile-function ast)))

(defun translate-lambda-list (lambda-list)
  (loop with ll with vars with args
        for item in lambda-list
        do (cond ((member item lambda-list-keywords)
                  (push item ll))
                 ((consp item)
                  (push (make-instance 'argument :rtype :object) args)
                  (push (make-instance 'argument :rtype :object) args)
                  (if (= (length item) 3)
                      (let ((keyv (find-or-create-variable
                                   (second item)))
                            (predv (find-or-create-variable
                                    (third item))))
                        (push (list (first item) keyv predv) ll)
                        (push keyv vars)
                        (push predv vars))
                      (let ((keyv (find-or-create-variable
                                   (first item)))
                            (predv (find-or-create-variable
                                    (second item))))
                        (push (list keyv predv) ll)
                        (push keyv vars)
                        (push predv vars))))
                 (t (let ((v (find-or-create-variable item)))
                      (push v ll)
                      (push v vars)
                      (push (make-instance 'argument :rtype :object) args))))
        finally (return (values (nreverse ll) vars args))))

(defun insert-initial-bindings (inserter vars args)
  (loop for var in vars for arg in args
        for setq = (make-instance 'writevar :variable var
                                  :inputs (list arg))
        do (before inserter setq)))

(defmethod compile-function ((ast cleavir-ast:function-ast))
  (multiple-value-bind (ll vars args)
      (translate-lambda-list (cleavir-ast:lambda-list ast))
    (let* ((return (make-instance 'returni))
           (f (make-instance 'function
                :allow-other-keys t :lambda-list ll :inputs args))
           (end (make-instance 'iblock :dynamic-environment f))
           (inserter (make-instance 'inserter :function f)))
      (setf (end f) end)
      (reset inserter end)
      (terminate inserter return)
      (setf (inputs return)
            (list (compile-ast (cleavir-ast:body-ast ast)
                               inserter :multiple-values)))
      (let ((start (iblock inserter)))
        (insert-initial-bindings inserter vars args)
        (finalize inserter)
        (setf (start f) start (inputs start) args))
      f)))

(defgeneric compile-ast (ast inserter context))

(defmethod compile-ast ((ast cleavir-ast:if-ast) inserter context)
  (check-type inserter inserter)
  (assert (one-successor-context-p context))
  (let* ((arg (unless (effect-context-p context)
                (make-instance 'argument :rtype context)))
         (iblock (iblock inserter))
         (tjump (make-instance 'jump :next (list iblock)))
         (tblock (make-instance 'iblock
                   :dynamic-environment (dynamic-environment iblock)
                   :end tjump))
         (tinserter (make-instance 'inserter
                      :function (function inserter)
                      :iblock tblock :insert-point tjump))
         (tvalue
           (compile-ast (cleavir-ast:then-ast ast)
                        tinserter context))
         (ejump (make-instance 'jump :next (list iblock)))
         (eblock (make-instance 'iblock
                   :dynamic-environment (dynamic-environment iblock)
                   :end ejump))
         (einserter (make-instance 'inserter
                      :function (function inserter)
                      :iblock eblock :insert-point ejump))
         (evalue
           (compile-ast (cleavir-ast:else-ast ast)
                        einserter context))
         (cblock (make-instance 'iblock
                   :dynamic-environment (dynamic-environment iblock))))
    (cond
      ((effect-context-p context)
       (setf (inputs tjump) nil
             (inputs ejump) nil))
      (t
       (assert (and (rtype= tvalue context)
                    (rtype= evalue context)))
       (setf (inputs tjump) (list tvalue)
             (inputs ejump) (list evalue)
             (inputs iblock) (list arg))))
    (setf (predecessors iblock) (make-set tblock eblock))
    (finalize inserter)
    (reset inserter cblock)
    (compile-ast (cleavir-ast:test-ast ast)
                 inserter (list tblock eblock))
    arg))

(defun block-info (block-ast) (gethash block-ast *block-info*))
(defun (setf block-info) (new-info block-ast)
  (setf (gethash block-ast *block-info*) new-info))

(defmethod compile-ast ((ast cleavir-ast:block-ast) inserter context)
  (check-type inserter inserter)
  (assert (one-successor-context-p context))
  (let* ((next (iblock inserter))
         (contvar (make-instance 'variable :rtype :continuation))
         (function (function inserter))
         (inputs
           (if (effect-context-p context)
               nil
               (list (make-instance 'argument :rtype context))))
         (main (make-instance 'iblock))
         (pre (make-instance 'iblock
                :dynamic-environment (dynamic-environment next)))
         (catch (make-instance 'catch :next (list main next)))
         (wcont (make-instance 'writevar
                  :variable contvar :inputs (list catch)))
         (lu (make-instance 'local-unwind :next (list next))))
    (setf (block-info ast) (list catch next function contvar context)
          (inputs next) inputs
          (dynamic-environment main) catch)
    (finalize inserter)
    (reset inserter main)
    (terminate inserter lu)
    (setf (inputs lu)
          (list (compile-ast (cleavir-ast:body-ast ast) inserter context)))
    (before inserter wcont)
    (finalize inserter)
    (reset inserter pre)
    (terminate pre catch)
    (if (effect-context-p context)
        (values)
        (first inputs))))

(defmethod compile-ast ((ast cleavir-ast:return-from-ast) inserter context)
  (check-type inserter inserter)
  (let* ((block-ast (cleavir-ast:block-ast ast))
         (iblock (iblock inserter))
         (new-iblock (make-instance 'iblock
                       :dynamic-environment (dynamic-environment iblock)))
         (function (function inserter)))
    (finalize inserter)
    (reset inserter new-iblock)
    (destructuring-bind (catch next bfunction contvar bcontext)
        (block-info block-ast)
      (if (eq function bfunction)
          ;; local
          (let ((lu (make-instance 'local-unwind)))
            (terminate inserter lu)
            (setf (inputs lu)
                  (list (compile-ast (cleavir-ast:form-ast ast)
                                     inserter bcontext))))
          ;; nonlocal
          (let ((u (make-instance 'unwind :catch catch :destination next))
                (rv (make-instance 'readvar
                      :variable contvar :rtype :continuation)))
            (setf (unwinds catch) (nset-adjoin u (unwinds catch)))
            (push new-iblock (entrances next))
            (terminate inserter u)
            (before inserter rv)
            (setf (inputs u)
                  (list* rv
                         (compile-ast (cleavir-ast:form-ast ast)
                                      inserter bcontext)))))))
  (no-return context))

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

(defun compile-sequence-for-effect (asts inserter)
  (loop for sub in (reverse asts)
        do (compile-ast sub inserter :effect)))

(defmethod compile-ast ((ast cleavir-ast:tagbody-ast) inserter context)
  (assert (effect-context-p context))
  (multiple-value-bind (prefix tags)
      (parse-tagbody (cleavir-ast:item-asts ast))
    (let* ((catch (make-instance 'catch))
           (contvar (make-instance 'variable :rtype :continuation))
           (wcont (make-instance 'writevar
                    :variable contvar :inputs (list catch)))
           (next (iblock inserter))
           (tag-iblocks
             (loop repeat (length tags)
                   collect (make-instance 'iblock :dynamic-environment catch)))
           (prefix-iblock (make-instance 'iblock :dynamic-environment catch))
           ;; This unconditionally jumps to prefix, but it has a different
           ;; dynamic environment, so it's a different block.
           ;; They might be merged later if the tagbody is all local.
           (before (make-instance 'iblock
                     :dynamic-environment (dynamic-environment next))))
      ;; Set up the tag infos
      (loop for (tag-ast) in tags
            for tag-iblock in tag-iblocks
            do (setf (go-info tag-ast)
                     (list catch tag-iblock (function inserter) contvar)))
      ;; Generate code
      (flet ((gen-body (inserter body nextb iblock)
               (let ((term (if (eq nextb next)
                               (make-instance 'local-unwind
                                 :inputs nil :next (list nextb))
                               (make-instance 'jump
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

(defmethod compile-ast ((ast cleavir-ast:go-ast) inserter context)
  (finalize inserter)
  (destructuring-bind (catch next bfunction contvar)
      (go-info (cleavir-ast:tag-ast ast))
    (let ((function (function inserter)))
      (if (eq function bfunction)
          (before inserter (make-instance 'local-unwind
                             :inputs () :next (list next)))
          (let ((rv (make-instance 'readvar
                      :rtype :continuation :variable contvar)))
            (before inserter (make-instance 'unwind
                               :inputs (list rv)
                               :catch catch :destination (list next)))
            (before inserter rv)))))
  (no-return context))

(defmethod compile-ast ((ast cleavir-ast:progn-ast) inserter context)
  (check-type inserter inserter)
  (assert (one-successor-context-p context))
  (let ((form-asts (cleavir-ast:form-asts ast)))
    (assert (not (null form-asts)))
    (let ((last (first (last form-asts)))
          (bl (butlast form-asts)))
      (prog1 (compile-ast last inserter context)
        (compile-sequence-for-effect bl inserter)))))

(defun compile-arguments (arg-asts inserter)
  (loop for arg-ast in (reverse arg-asts)
        collect (compile-ast arg-ast inserter :object)))

(defmethod compile-ast ((ast cleavir-ast:call-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (one-successor-context-p context))
  (let* ((call (make-instance 'call))
         (_ (before inserter call))
         (callee (cleavir-ast:callee-ast ast))
         (calleev (compile-ast callee inserter :value))
         (args (cleavir-ast:argument-asts ast))
         (argsvs (compile-arguments args inserter)))
    (declare (ignore _))
    (assert (every (lambda (a) (eq (rtype a) :object)) argsvs))
    (setf (inputs call) (list* calleev argsvs))
    (prog1 (figure-values inserter call context)
      (before inserter call))))

(defmethod compile-ast ((ast cleavir-ast:function-ast)
                        inserter context)
  (check-type inserter inserter)
  ;; NOTE: Could just do nothing if :effect.
  (assert (one-successor-context-p context))
  (let* ((f (or (gethash ast *function-info*)
                (setf (gethash ast *function-info*)
                      (compile-function ast))))
         (enclose (make-instance 'enclose :code f)))
    (prog1 (figure-values inserter enclose context)
      (before inserter enclose))))

(defmethod compile-ast ((ast cleavir-ast:setq-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (eq context :effect))
  (let* ((var (find-or-create-variable (cleavir-ast:lhs-ast ast)))
         (assign (make-instance 'writevar :variable var)))
    (before inserter assign)
    (let ((v (compile-ast (cleavir-ast:value-ast ast) inserter :object)))
      (setf (inputs assign) (list v))))
  (values))

(defmethod compile-ast ((ast cleavir-ast:multiple-value-setq-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (eq context :effect))
  (let* ((lhs-asts (cleavir-ast:lhs-asts ast))
         (wvs
           (loop for lhs in lhs-asts
                 for wv = (make-instance 'writevar :variable lhs)
                 collect (before inserter wv)))
         (extracts
           (loop for lhs in lhs-asts
                 for var = (find-or-create-variable lhs)
                 for i from 0
                 for extract = (make-instance 'extract :index i)
                 collect (before inserter extract)))
         (rtype (make-aggregate (length lhs-asts) :object))
         (val (compile-ast (cleavir-ast:form-ast ast)
                           inserter rtype)))
    (loop for wv in wvs
          for extract in extracts
          do (setf (inputs extract) (list val)
                   (inputs wv) (list extract))))
  (values))

(defmethod compile-ast ((ast cleavir-ast:unreachable-ast)
                        inserter context)
  (check-type inserter inserter)
  (finalize inserter)
  (let ((next (iblock inserter)))
    (reset inserter (make-instance 'iblock
                      :dynamic-environment (dynamic-environment next)))
    (terminate inserter (make-instance 'unreachable)))
  (no-return context))

(defmethod compile-ast ((ast cleavir-ast:lexical-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (one-successor-context-p context))
  (let ((rv (make-instance 'readvar
              :variable (find-or-create-variable ast))))
    (prog1 (figure-values inserter rv context)
      (before inserter rv))))

(defmethod compile-ast ((ast cleavir-ast:values-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (one-successor-context-p context))
  (let ((arg-asts (cleavir-ast:argument-asts ast)))
    (if (effect-context-p context)
        (compile-sequence-for-effect arg-asts inserter)
        (let* ((rt (make-aggregate (length arg-asts) :object))
               (create (make-instance 'create :rtype rt))
               (args (compile-arguments arg-asts inserter)))
          (setf (inputs create) args)
          (prog1 (figure-values inserter create context)
            (before inserter create))))))

(defmethod compile-ast ((ast cleavir-ast:eq-ast)
                        inserter context)
  (check-type inserter inserter)
  (assert (n-next-context-p context 2))
  (let ((e (make-instance 'eqi :next context)))
    (terminate inserter e)
    (let ((args (compile-arguments
                 (list (cleavir-ast:arg1-ast ast)
                       (cleavir-ast:arg2-ast ast))
                 inserter)))
      (setf (inputs e) args)))
  (values))

(macrolet ((defprimop (primop ast &rest readers)
             (let* ((info (or (gethash primop *primops*)
                              (error "BUG: No primop: ~a" primop)))
                    (nv-p (null (rtype info))))
               `(defmethod compile-ast ((ast ,ast) inserter context)
                  (check-type inserter inserter)
                  (assert
                   ,(if nv-p
                        '(effect-context-p context)
                        '(one-successor-context-p context)))
                  (let ((p (before
                            inserter
                            (make-instance ',(if nv-p 'nvprimop 'vprimop)
                              :primop-info ',info))))
                    (setf (inputs p)
                          (compile-arguments
                           (list ,@(loop for reader in readers
                                         collect `(,reader ast)))
                           inserter))
                    p)))))
  (defprimop cleavir-primop:car cleavir-ast:car-ast cleavir-ast:cons-ast)
  (defprimop cleavir-primop:cdr cleavir-ast:cdr-ast cleavir-ast:cons-ast)
  (defprimop cleavir-primop:rplaca cleavir-ast:rplaca-ast
    cleavir-ast:cons-ast cleavir-ast:object-ast)
  (defprimop cleavir-primop:rplacd cleavir-ast:rplacd-ast
    cleavir-ast:cons-ast cleavir-ast:object-ast))
