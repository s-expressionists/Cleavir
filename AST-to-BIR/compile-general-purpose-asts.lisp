(in-package #:cleavir-ast-to-bir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPILE-FUNCTION

(defun bind-lexical-as-argument (lexical-variable function)
  (assert (not (nth-value 1 (gethash lexical-variable *variables*)))
          ()
          "Lexical variable ~a bound to ~a already."
          lexical-variable (gethash lexical-variable *variables*))
  (setf (gethash lexical-variable *variables*)
        (make-instance 'bir:argument
          :name (ast:name lexical-variable)
          :function function)))

(defun bind-lambda-list-arguments (lambda-list function)
  (loop for item in lambda-list
        collect (cond ((member item lambda-list-keywords)
                       item)
                      ((consp item)
                       (if (= (length item) 3)
                           (list (first item)
                                 (bind-lexical-as-argument (second item)
                                                           function)
                                 (bind-lexical-as-argument (third item)
                                                           function))
                           (list (bind-lexical-as-argument (first item)
                                                           function)
                                 (bind-lexical-as-argument (second item)
                                                           function))))
                      (t (bind-lexical-as-argument item function)))))

(defmethod compile-function ((ast ast:function-ast) system)
  (let* ((module *current-module*)
         (function (make-instance 'bir:function
                     :name (ast:name ast)
                     :docstring (ast:docstring ast)
                     :original-lambda-list (ast:original-lambda-list ast)
                     :origin (ast:origin ast)
                     :policy (ast:policy ast)
                     :attributes (ast:attributes ast)
                     :module module))
         (inserter (make-instance 'inserter))
         (start (make-iblock inserter
                             :name (symbolicate (write-to-string (ast:name ast))
                                                '#:-start)
                             :function function :dynamic-environment function)))
    (set:nadjoinf (bir:functions module) function)
    (let ((lambda-list (bind-lambda-list-arguments (ast:lambda-list ast)
                                                   function)))
      (setf (bir:lambda-list function) lambda-list))
    (setf (bir:start function) start)
    (begin inserter start)
    (let ((rv (compile-ast (ast:body-ast ast) inserter system)))
      (cond
        ((eq rv :no-return)
         (setf (bir:returni function) nil))
        (t
         (let ((returni (make-instance 'bir:returni
                          :inputs rv)))
           (setf (bir:returni function) returni)
           (terminate inserter returni)))))
    (bir:compute-iblock-flow-order function)
    function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IF-AST

(defun compile-branch (inserter system test-ast branch-asts)
  (let ((iblocks (compile-test-ast test-ast inserter system)))
    (when (eq iblocks :no-return) (return-from compile-branch iblocks))
    (assert (= (length iblocks) (length branch-asts)))
    (let ((map (loop with r = nil
                     for iblock in iblocks
                     for ast in branch-asts
                     for ins = (make-instance 'inserter)
                     do (proceed ins iblock)
                        (let ((rv (compile-ast ast ins system)))
                          (unless (eq rv :no-return)
                            (push (list ins (iblock ins) rv) r)))
                     finally (return (nreverse r)))))
      (case (length map)
        ((0) ; no branch returned, so neither do we
         :no-return)
        ((1) ; a single branch returned, so don't bother with a merge
         (destructuring-bind (_ block rv) (first map)
           (declare (ignore _))
           (proceed inserter block)
           rv))
        (t ; multiple blocks, so we have to merge their results
         (let ((mergeb (make-iblock inserter :name '#:merge)))
           ;; Dump everything into multiple-values.
           (let ((phi (make-instance 'bir:phi :iblock mergeb)))
             (loop for (ins nil rv) in map
                   do (terminate
                       ins
                       (make-instance 'bir:jump
                         :inputs rv :outputs (list phi)
                         :next (list mergeb))))
             (setf (bir:inputs mergeb) (list phi))
             (begin inserter mergeb)
             (list phi))))))))

(defmethod compile-ast ((ast ast:if-ast) inserter system)
  (compile-branch inserter system (ast:test-ast ast)
                  (list (ast:then-ast ast) (ast:else-ast ast))))

(defmethod compile-test-ast (ast inserter system)
  (with-compiled-asts (test (ast) inserter system)
    (let ((tblock (make-iblock inserter :name '#:if-then))
          (eblock (make-iblock inserter :name '#:if-else)))
      (terminate inserter 'bir:ifi
                 :inputs test :next (list tblock eblock))
      (list tblock eblock))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BRANCH-AST

(defmethod compile-ast ((ast ast:branch-ast) inserter system)
  (compile-branch inserter system (ast:test-ast ast)
                  (append (ast:branch-asts ast)
                          (list (ast:default-ast ast)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PROGN-AST

(defmethod compile-ast ((ast ast:progn-ast) inserter system)
  (let ((form-asts (ast:form-asts ast)))
    (assert (not (null form-asts)))
    (let ((last (first (last form-asts)))
          (bl (butlast form-asts)))
      (if (compile-sequence-for-effect bl inserter system)
          (compile-ast last inserter system)
          :no-return))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BLOCK-AST

(defun block-info (block-ast) (gethash block-ast *block-info*))
(defun (setf block-info) (new-info block-ast)
  (setf (gethash block-ast *block-info*) new-info))

(defun insert-unwind (inserter come-from dest &optional inputs outputs)
  (let ((uw (make-instance 'bir:unwind
              :inputs inputs :outputs outputs :come-from come-from
              :destination dest)))
    (terminate inserter uw)
    (set:nadjoinf (bir:unwinds come-from) uw)
    (set:nadjoinf (bir:entrances dest) (iblock inserter)))
  (values))

(defmethod compile-ast ((ast ast:block-ast) inserter system)
  (let* ((function (function inserter))
         (de (dynamic-environment inserter))
         (during (make-iblock inserter
                              :name (symbolicate '#:block-
                                                 (ast:name ast))))
         (mergeb (make-iblock inserter
                              :name (symbolicate
                                     '#:block-
                                     (ast:name ast)
                                     '#:-merge)
                              :function function
                              :dynamic-environment de))
         (phi (make-instance 'bir:phi :iblock mergeb))
         (come-from (make-instance 'bir:come-from
                      :next (list during mergeb)
                      :name (ast:name ast))))
    (set:nadjoinf (bir:come-froms function) come-from)
    (setf (bir:inputs mergeb) (list phi))
    (setf (bir:dynamic-environment during) come-from)
    (terminate inserter come-from)
    (begin inserter during)
    (setf (block-info ast) (list function come-from mergeb))
    (let ((normal-rv (compile-ast (ast:body-ast ast) inserter system)))
      (unless (eq normal-rv :no-return)
        (terminate inserter 'bir:jump
                   :inputs normal-rv
                   :outputs (copy-list (bir:inputs mergeb))
                   :next (list mergeb))))
    (begin inserter mergeb)
    (list phi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RETURN-FROM-AST

(defmethod compile-ast ((ast ast:return-from-ast) inserter system)
  (let ((rv (compile-ast (ast:form-ast ast) inserter system)))
    (unless (eq rv :no-return)
      (destructuring-bind (function come-from mergeb)
          (block-info (ast:block-ast ast))
        (if (eq function (function inserter))
            ;; local
            (terminate inserter 'bir:jump
                       :inputs rv
                       :outputs (copy-list (bir:inputs mergeb))
                       :next (list mergeb))
            ;; nonlocal
            (insert-unwind inserter come-from mergeb rv
                           (copy-list (bir:inputs mergeb)))))))
  :no-return)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TAGBODY-AST

(defun go-info (tag-ast)
  (gethash tag-ast *go-info*))

(defun (setf go-info) (new-info tag-ast)
  (setf (gethash tag-ast *go-info*) new-info))

(defmethod compile-ast ((ast ast:tagbody-ast) inserter system)
  (let ((prefix-ast (ast:prefix-ast ast))
        (item-asts (ast:item-asts ast)))
    ;; Special case: there are no tags; treat this as a progn except
    ;; that it does not return values.
    (when (null item-asts)
      (return-from compile-ast
        (if (eq (compile-ast prefix-ast inserter system) :no-return)
            :no-return
            ())))
    ;; General case
    (let* ((old-dynenv (dynamic-environment inserter))
           (function (function inserter))
           (prefix-iblock (make-iblock inserter :name '#:tagbody))
           (tag-iblocks
             (loop for tag-ast in item-asts
                   ;; name could be an integer, so write it out
                   for tagname = (write-to-string (ast:name tag-ast))
                   for bname = (symbolicate '#:tag- tagname)
                   collecting (make-iblock inserter :name bname)))
           (come-from (make-instance 'bir:come-from
                        :next (list* prefix-iblock tag-iblocks))))
      (set:nadjoinf (bir:come-froms function) come-from)
      ;; this is used to check whether the come-from is actually necessary.
      (setf (go-info come-from) nil)
      (setf (bir:dynamic-environment prefix-iblock) come-from)
      (loop for tag-ast in item-asts
            for tag-iblock in tag-iblocks
            do (setf (bir:dynamic-environment tag-iblock) come-from
                     (go-info tag-ast) (list come-from tag-iblock function)))
      (terminate inserter come-from)
      (begin inserter prefix-iblock)
      (unless (eq (compile-ast prefix-ast inserter system) :no-return)
        (terminate inserter 'bir:jump
                   :inputs () :outputs ()
                   :next (list (first tag-iblocks))))
      (loop for tag-ast in item-asts
            for body-ast = (ast:body-ast tag-ast)
            for (ib . rest) on tag-iblocks
            do (begin inserter ib)
            if (eq (compile-ast body-ast inserter system) :no-return)
              ;; Code doesn't return. If this is the last tag, that means the
              ;; tagbody doesn't either.
              do (unless rest (return-from compile-ast :no-return))
                 ;; Code continues onto the next tag, or out of the tagbody.
            else do (let ((next
                            (if rest
                                (first rest)
                                (make-iblock inserter
                                             :name '#:tagbody-resume
                                             :dynamic-environment
                                             old-dynenv))))
                      (terminate inserter 'bir:jump
                                 :inputs () :outputs ()
                                 :next (list next))
                      (unless rest
                        ;; Start on the block after the tagbody.
                        (begin inserter next)
                        ;; We return no values.
                        (return-from compile-ast :no-value)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GO-AST

(defmethod compile-ast ((ast ast:go-ast) inserter system)
  (declare (ignore system))
  (destructuring-bind (come-from iblock cfunction) (go-info (ast:tag-ast ast))
    (let ((function (function inserter)))
      (cond
        ((eq function cfunction)
         ;; local
         (terminate inserter 'bir:jump
                    :inputs () :outputs ()
                    :next (list iblock)))
        (t
         (setf (go-info come-from) t)
         ;; nonlocal
         (insert-unwind inserter come-from iblock)))))
  :no-return)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UNWIND-PROTECT-AST

(defmethod compile-ast ((ast ast:unwind-protect-ast) inserter system)
  (with-compiled-ast (fu (ast:cleanup-ast ast) inserter system)
    (let* ((uw (make-instance 'bir:unwind-protect :inputs fu))
           (ode (dynamic-environment inserter))
           (during (make-iblock inserter :dynamic-environment uw)))
      (setf (bir:next uw) (list during))
      (terminate inserter uw)
      (begin inserter during)
      (with-compiled-ast (rv (ast:body-ast ast) inserter system)
        ;; Pass the return values through a phi so that they're easy for
        ;; the client to get at. KLUDGEy.
        (let* ((next (make-iblock inserter :dynamic-environment ode))
               (phi (make-instance 'bir:phi :iblock next)))
          (setf (bir:inputs next) (list phi))
          (terminate inserter 'bir:jump
                     :inputs rv :outputs (list phi) :next (list next))
          (begin inserter next)
          (list phi))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALL-AST

(defmethod compile-ast ((ast ast:call-ast) inserter system)
  (with-compiled-ast (callee (ast:callee-ast ast) inserter system)
    (with-compiled-arguments (args (ast:argument-asts ast) inserter system)
      (let ((call-out (make-instance 'bir:output)))
        (insert inserter 'bir:call
                :inputs (list* (first callee) args)
                :outputs (list call-out))
        (list call-out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INLINE-AST

(defmethod compile-ast ((ast ast:inline-ast) inserter system)
  ;; We sometimes see the same inlined function AST at multiple locations in
  ;; the code. To ensure things work we rebind these; it's ok since inline ASTs
  ;; are never going to refer to anything from the surrounding context.
  (let ((*variables* (make-hash-table :test #'eq))
        (*block-info* (make-hash-table :test #'eq))
        (*go-info* (make-hash-table :test #'eq))
        (*inlined-at* (inline-origin (ast:origin ast) *inlined-at* system)))
    (compile-ast (ast:body-ast ast) inserter system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTION-AST

(defmethod compile-ast ((ast ast:function-ast) inserter system)
  (let* ((f (compile-function ast system))
         (enclose-out (make-instance 'bir:output :name (bir:name f)))
         (enclose (make-instance 'bir:enclose
                    :code f :outputs (list enclose-out))))
    (setf (bir:enclose f) enclose)
    (insert inserter enclose)
    (list enclose-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LEXICAL-BIND-AST

(defmethod compile-ast ((ast ast:lexical-bind-ast) inserter system)
  (with-compiled-ast (rv (ast:value-ast ast) inserter system)
    (let* ((var (bind-variable (ast:lexical-variable ast) (ast:ignore ast)))
           (leti (make-instance 'bir:leti
                   :inputs rv :outputs (list var))))
      (adjoin-variable inserter var)
      (insert inserter leti)
      (setf (bir:binder var) leti))
    ;; return no values
    :no-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SETQ-AST

(defmethod compile-ast ((ast ast:setq-ast) inserter system)
  (let ((var (find-variable (ast:lexical-variable ast))))
    (bir:record-variable-set var)
    (with-compiled-ast (rv (ast:value-ast ast) inserter system)
      (insert inserter 'bir:writevar
              :inputs rv :outputs (list var))
      (let ((readvar-out (make-instance 'bir:output :name (bir:name var))))
        (insert inserter 'bir:readvar
                :inputs (list var) :outputs (list readvar-out))
        (list readvar-out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DYNAMIC-BIND-AST

(defmethod compile-ast ((ast ast:dynamic-bind-ast) inserter system)
  (with-compiled-asts (args ((ast:name-ast ast) (ast:value-ast ast))
                            inserter system)
    (let* ((during (make-iblock inserter))
           (ode (dynamic-environment inserter))
           (bind (make-instance 'bir:bind :inputs args :next (list during))))
      (setf (bir:dynamic-environment during) bind)
      (terminate inserter bind)
      (begin inserter during)
      (with-compiled-ast (rv (cleavir-ast:body-ast ast) inserter system)
        (let ((next (make-iblock inserter :dynamic-environment ode)))
          (terminate inserter 'bir:jump
                     :inputs () :outputs () :next (list next))
          (begin inserter next))
        rv))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; THE-AST

;;; Wrap THEI on LINEAR-DATUM. Don't try and intersect the type
;;; assertion onto an existing THEI, since we'd like to keep the
;;; runtime operation of the assertions separate. But do avoid
;;; wrapping THEI when the derived type of LINEAR-DATUM is a subtype
;;; of the asserted type, since we can prove from the get-go that the
;;; type assertion is never needed.
(defun wrap-thei (inserter linear-datum asserted-type
                  type-check-function system)
  (if (ctype:values-subtypep (bir:ctype linear-datum) asserted-type system)
      linear-datum
      (let ((thei-out (make-instance 'bir:output
                        :name (bir:name linear-datum))))
        (insert inserter 'bir:thei
                :inputs (list linear-datum)
                :outputs (list thei-out)
                :asserted-type asserted-type
                :type-check-function type-check-function)
        thei-out)))

(defmethod compile-ast ((ast ast:the-ast) inserter system)
  (let* ((inner (ast:form-ast ast))
         (ctype (ast:ctype ast))
         (type-check-function-ast (ast:type-check-function-ast ast))
         (required (ctype:values-required ctype system))
         (rv (compile-ast inner inserter system))
         (type-check-function
           (if (symbolp type-check-function-ast)
               type-check-function-ast
               (compile-function type-check-function-ast system))))
    (cond ((or (eq rv :no-return))
           :no-return)
          ((not (symbolp type-check-function))
           ;; We do an mv-call here - see generate-type-checks.lisp
           ;; so we force receiving and outputting multiple values.
           (let ((out (make-instance 'bir:output
                        :name (bir:name (first rv)))))
             (insert inserter 'bir:thei
                     :inputs rv :outputs (list out)
                     :asserted-type ctype
                     :type-check-function type-check-function)
             (list out)))
          ((some (lambda (ctype) (ctype:bottom-p ctype system)) required)
           (terminate inserter 'bir:unreachable)
           :no-return)
          (t ; arbitrary values
           (list (wrap-thei inserter (first rv)
                            ctype type-check-function system))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DYNAMIC-ALLOCATION-AST

(defmethod compile-ast ((ast ast:dynamic-allocation-ast)
                        inserter system)
  (compile-ast (ast:form-ast ast) inserter system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO
;;; CONSTANT-SYMBOL-VALUE-AST
;;; SET-CONSTANT-SYMBOL-VALUE-AST
;;; CONSTANT-FDEFINITION-AST

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPEQ-AST

(defmethod compile-test-ast ((ast ast:typeq-ast) inserter system)
  (with-compiled-ast (obj (ast:form-ast ast) inserter system)
    (let* ((tspec (ast:test-ctype ast))
           ;; FIXME: Will get weird with custom ctypes.
           (tspec-str (write-to-string tspec))
           (tblock (make-iblock inserter
                                :name (symbolicate
                                       '#:typeq- tspec-str '#:-then)))
           (eblock (make-iblock inserter
                                :name (symbolicate
                                       '#:typeq- tspec-str '#:-else)))
           (tq-out (make-instance 'bir:output
                     :name (symbolicate
                            '#:typeq- tspec-str '#:-result)))
           (tq (make-instance 'bir:typeq-test
                 :inputs obj :outputs (list tq-out)
                 :test-ctype tspec)))
      (insert inserter tq)
      (terminate inserter 'bir:ifi
                 :inputs (list tq-out)
                 :next (list tblock eblock))
      (list tblock eblock))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UNREACHABLE-AST

(defmethod compile-ast ((ast ast:unreachable-ast) inserter system)
  (declare (ignore system))
  (terminate inserter 'bir:unreachable)
  :no-return)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LEXICAL-AST

(defmethod compile-ast ((ast ast:lexical-ast) inserter system)
  (declare (ignore system))
  (let ((var (find-variable (ast:lexical-variable ast))))
    ;; FIXME: We probably want to make a new AST class to distinguish between
    ;; these two cases more cleanly.
    (typecase var
      (bir:argument (list var))
      (t
       (bir:record-variable-ref var)
       (let ((readvar-out (make-instance 'bir:output :name (bir:name var))))
         (insert inserter 'bir:readvar
                 :inputs (list var) :outputs (list readvar-out))
         (list readvar-out))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EQ-AST

(defmethod compile-test-ast ((ast ast:eq-ast) inserter system)
  (with-compiled-asts (args ((ast:arg1-ast ast) (ast:arg2-ast ast))
                            inserter system)
    (let ((tblock (make-iblock inserter :name '#:eq-then))
          (eblock (make-iblock inserter :name '#:eq-else)))
      (let* ((eq-out (make-instance 'bir:output
                       :name '#:eq-result))
             (eq-test (make-instance 'bir:eq-test
                        :inputs args :outputs (list eq-out))))
        (insert inserter eq-test)
        (terminate inserter 'bir:ifi
                   :inputs (list eq-out)
                   :next (list tblock eblock)))
      (list tblock eblock))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NEQ-AST

(defmethod compile-test-ast ((ast ast:neq-ast) inserter system)
  (with-compiled-asts (args ((ast:arg1-ast ast) (ast:arg2-ast ast))
                            inserter system)
    (let ((tblock (make-iblock inserter :name '#:neq-then))
          (eblock (make-iblock inserter :name '#:neq-else)))
      (let* ((eq-out (make-instance 'bir:output
                       :name '#:neq-result))
             (eq-test (make-instance 'bir:eq-test
                        :inputs args :outputs (list eq-out))))
        (insert inserter eq-test)
        (terminate inserter 'bir:ifi
                   :inputs (list eq-test) :next (list eblock tblock)))
      (list tblock eblock))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CASE-AST

(defmethod compile-test-ast ((ast ast:case-ast) inserter system)
  (with-compiled-ast (obj (ast:arg-ast ast) inserter system)
    (let* ((comparees (ast:comparees ast))
           (iblocks (loop repeat (1+ (length comparees))
                          collect (make-iblock inserter))))
      (terminate inserter 'bir:case
                 :inputs obj
                 :comparees comparees :next iblocks)
      iblocks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONSTANT-AST

(defmethod compile-ast ((ast ast:constant-ast) inserter system)
  (declare (ignore system))
  (let ((const (bir:constant-in-module (ast:value ast) *current-module*))
        (constref-out (make-instance 'bir:output)))
    (insert inserter 'bir:constant-reference
            :inputs (list const) :outputs (list constref-out))
    (list constref-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LOAD-TIME-VALUE-AST

(defmethod compile-ast ((ast ast:load-time-value-ast) inserter system)
  (declare (ignore system))
  (let ((ltv (bir:load-time-value-in-module
              (ast:form ast) (ast:read-only-p ast)
              *current-module*))
        (ltv-out (make-instance 'bir:output)))
   (insert inserter 'bir:load-time-value-reference
           :inputs (list ltv) :outputs (list ltv-out))
    (list ltv-out)))
