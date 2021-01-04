(in-package #:cleavir-ast-to-bir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPILE-FUNCTION

(defun bind-lexical-ast-as-argument (lexical-ast)
  (assert (not (nth-value 1 (gethash lexical-ast *variables*))))
  (setf (gethash lexical-ast *variables*)
        (make-instance 'cleavir-bir:argument
                       :name (cleavir-ast:name lexical-ast)
                       :rtype :object)))

(defun bind-lambda-list-arguments (lambda-list)
  (loop for item in lambda-list
        collect (cond ((member item lambda-list-keywords)
                       item)
                      ((consp item)
                       (if (= (length item) 3)
                           (list (first item)
                                 (bind-lexical-ast-as-argument (second item))
                                 (bind-lexical-ast-as-argument (third item)))
                           (list (bind-lexical-ast-as-argument (first item))
                                 (bind-lexical-ast-as-argument (second item)))))
                      (t (bind-lexical-ast-as-argument item)))))

(defmethod compile-function ((ast cleavir-ast:function-ast) system)
  (let* ((module *current-module*)
         (function (make-instance 'cleavir-bir:function
                     :name (cleavir-ast:name ast)
                     :docstring (cleavir-ast:docstring ast)
                     :original-lambda-list (cleavir-ast:original-lambda-list ast)
                     :origin (cleavir-ast:origin ast)
                     :policy (cleavir-ast:policy ast)
                     :module module))
         (inserter (make-instance 'inserter))
         (start (make-iblock inserter
                             :name (symbolicate (write-to-string
                                                 (cleavir-ast:name ast))
                                                '#:-start)
                             :function function :dynamic-environment function)))
    (cleavir-set:nadjoinf (cleavir-bir:functions module) function)
    (let ((lambda-list (bind-lambda-list-arguments
                        (cleavir-ast:lambda-list ast))))
      (setf (cleavir-bir:lambda-list function) lambda-list)
      ;; Derive the type of the &rest argument to be LIST.
      (cleavir-bir:map-lambda-list
       (lambda (state item index)
         (declare (ignore index))
         (when (eq state '&rest)
           (cleavir-bir:derive-type-for-linear-datum
            item
            (let ((top-ctype (cleavir-ctype:top system)))
              ;; LIST is of course (or null cons)
              (cleavir-ctype:disjoin/2
               (cleavir-ctype:null-type system)
               (cleavir-ctype:cons top-ctype top-ctype system)
               system)))))
       lambda-list))
    (setf (cleavir-bir:start function) start)
    (begin inserter start)
    (let ((rv (compile-ast (cleavir-ast:body-ast ast) inserter system)))
      (cond
        ((eq rv :no-return)
         (setf (cleavir-bir:returni function) nil))
        (t
         (let ((returni (make-instance 'cleavir-bir:returni
                          :inputs (adapt inserter rv :multiple-values))))
           (setf (cleavir-bir:returni function) returni)
           (terminate inserter returni)))))
    (cleavir-bir:compute-iblock-flow-order function)
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
           (if (let ((first-rv (third (first map))))
                 (and (listp first-rv)
                      (loop with len = (length first-rv)
                            for (_0 _1 rv) in (rest map)
                            always (and (listp rv) (= (length rv) len)))))
               ;; All the same number of values, so we can phi them.
               (let* ((nrtypes (length (third (first map))))
                      ;; FIXME: In the future we may have non-:objects.
                      ;; It would be nice to not force everything to be objects
                      ;; in that circumstance.
                      ;; (Presumably for non-matches we'd cast both to :object)
                      (rtypes (make-list nrtypes :initial-element :object))
                      (phis (loop repeat nrtypes
                                  collect (make-instance 'cleavir-bir:phi
                                            :iblock mergeb :rtype :object))))
                 (loop for (ins ib rv) in map
                       do (terminate
                           ins
                           (make-instance 'cleavir-bir:jump
                             :inputs (adapt ins rv rtypes)
                             :outputs (copy-list phis)
                             :next (list mergeb))))
                 (setf (cleavir-bir:inputs mergeb) phis)
                 (begin inserter mergeb)
                 phis)
               ;; Dump everything into multiple-values.
               (let ((phi (make-instance 'cleavir-bir:phi
                            :iblock mergeb :rtype :multiple-values)))
                 (loop for (ins ib rv) in map
                       do (terminate
                           ins
                           (make-instance 'cleavir-bir:jump
                             :inputs (adapt ins rv :multiple-values)
                             :outputs (list phi)
                             :next (list mergeb))))
                 (setf (cleavir-bir:inputs mergeb) (list phi))
                 (begin inserter mergeb)
                 phi))))))))

(defmethod compile-ast ((ast cleavir-ast:if-ast) inserter system)
  (compile-branch inserter system (cleavir-ast:test-ast ast)
                  (list (cleavir-ast:then-ast ast) (cleavir-ast:else-ast ast))))

(defmethod compile-test-ast (ast inserter system)
  (with-compiled-asts (test (ast) inserter system (:object))
    (let ((tblock (make-iblock inserter :name '#:if-then))
          (eblock (make-iblock inserter :name '#:if-else)))
      (terminate inserter (make-instance 'cleavir-bir:ifi
                            :inputs test :next (list tblock eblock)))
      (list tblock eblock))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BRANCH-AST

(defmethod compile-ast ((ast cleavir-ast:branch-ast) inserter system)
  (compile-branch inserter system (cleavir-ast:test-ast ast)
                  (append (cleavir-ast:branch-asts ast)
                          (list (cleavir-ast:default-ast ast)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PROGN-AST

(defmethod compile-ast ((ast cleavir-ast:progn-ast) inserter system)
  (let ((form-asts (cleavir-ast:form-asts ast)))
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

(defun insert-unwind (inserter catch dest &optional inputs outputs)
  (let ((uw (make-instance 'cleavir-bir:unwind
              :inputs inputs :outputs outputs :catch catch
              :destination dest)))
    (terminate inserter uw)
    (cleavir-set:nadjoinf (cleavir-bir:unwinds catch) uw)
    (cleavir-set:nadjoinf (cleavir-bir:entrances dest) (iblock inserter)))
  (values))

(defmethod compile-ast ((ast cleavir-ast:block-ast) inserter system)
  (let* ((function (function inserter))
         (de (dynamic-environment inserter))
         (during (make-iblock inserter
                              :name (symbolicate '#:block-
                                                 (cleavir-ast:name ast))))
         (mergeb (make-iblock inserter
                              :name (symbolicate
                                     '#:block-
                                     (cleavir-ast:name ast)
                                     '#:-merge)
                              :function function
                              :dynamic-environment de))
         ;; KLUDGE: We force everything into multiple values as clients may
         ;; not be able to nonlocal-return in other formats.
         ;; Should be customizable.
         (phi (make-instance 'cleavir-bir:phi :rtype :multiple-values
                             :iblock mergeb))
         (catch (make-instance 'cleavir-bir:catch
                  :next (list during mergeb)
                  :name (cleavir-ast:name ast))))
    (cleavir-set:nadjoinf (cleavir-bir:catches function) catch)
    (setf (cleavir-bir:inputs mergeb) (list phi))
    (setf (cleavir-bir:dynamic-environment during) catch)
    (terminate inserter catch)
    (begin inserter during)
    (setf (block-info ast) (list function catch mergeb))
    (let ((normal-rv (compile-ast (cleavir-ast:body-ast ast) inserter system)))
      (unless (eq normal-rv :no-return)
        (terminate inserter
                   (make-instance 'cleavir-bir:jump
                     :inputs (adapt inserter normal-rv :multiple-values)
                     :outputs (cleavir-bir:inputs mergeb)
                     :next (list mergeb)))))
    (begin inserter mergeb)
    phi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RETURN-FROM-AST

(defmethod compile-ast ((ast cleavir-ast:return-from-ast) inserter system)
  (let ((rv (compile-ast (cleavir-ast:form-ast ast) inserter system)))
    (unless (eq rv :no-return)
      (destructuring-bind (function catch mergeb)
          (block-info (cleavir-ast:block-ast ast))
        (if (eq function (function inserter))
            ;; local
            (terminate
             inserter
             (make-instance 'cleavir-bir:jump
               :inputs (adapt inserter rv :multiple-values)
               :outputs (cleavir-bir:inputs mergeb)
               :next (list mergeb)))
            ;; nonlocal
            (insert-unwind inserter catch mergeb
                           (adapt inserter rv :multiple-values)
                           (cleavir-bir:inputs mergeb))))))
  :no-return)

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
             (setf current nil)
             (setf state item)
        else do (push item current)
        finally (if (vectorp state)
                    (setf prefix (nreverse current))
                    (push (cons state (nreverse current)) tags))
                (return (values prefix (nreverse tags)))))

(defmethod compile-ast ((ast cleavir-ast:tagbody-ast) inserter system)
  (multiple-value-bind (prefix tags)
      (parse-tagbody (cleavir-ast:item-asts ast))
    ;; Special case: there are no tags.
    (when (null tags)
      (return-from compile-ast
        (if (compile-sequence-for-effect prefix inserter system)
            ()
            :no-return)))
    ;; General case
    (let* ((old-dynenv (dynamic-environment inserter))
           (function (function inserter))
           (prefix-iblock (make-iblock inserter :name '#:tagbody))
           (tag-iblocks
             (loop for (tag-ast) in tags
                   ;; name could be an integer, so write it out
                   for tagname = (write-to-string (cleavir-ast:name tag-ast))
                   for bname = (symbolicate '#:tag- tagname)
                   collecting (make-iblock inserter :name bname)))
           (catch (make-instance 'cleavir-bir:catch
                    :next (list* prefix-iblock tag-iblocks))))
      (cleavir-set:nadjoinf (cleavir-bir:catches function) catch)
      ;; this is used to check whether the catch is actually necessary.
      (setf (go-info catch) nil)
      (setf (cleavir-bir:dynamic-environment prefix-iblock) catch)
      (loop for (tag-ast) in tags
            for tag-iblock in tag-iblocks
            do (setf (cleavir-bir:dynamic-environment tag-iblock) catch
                     (go-info tag-ast)
                     (list catch tag-iblock function)))
      (terminate inserter catch)
      (begin inserter prefix-iblock)
      (when (compile-sequence-for-effect prefix inserter system)
        (terminate inserter (make-instance 'cleavir-bir:jump
                              :inputs () :outputs ()
                              :next (list (first tag-iblocks)))))
      (loop for (tag . body) in tags
            for (ib . rest) on tag-iblocks
            do (begin inserter ib)
            if (compile-sequence-for-effect body inserter system)
              ;; Code continues onto the next tag, or out of the tagbody.
              do (let ((next
                         (if rest
                             (first rest)
                             (make-iblock inserter
                                          :name '#:tagbody-resume
                                          :dynamic-environment old-dynenv))))
                   (terminate inserter
                              (make-instance 'cleavir-bir:jump
                                :inputs () :outputs ()
                                :next (list next)))
                   (unless rest
                     ;; Start on the block after the tagbody.
                     (begin inserter next)
                     ;; We return no values.
                     (return-from compile-ast ())))
            else
              ;; Code doesn't return. If this is the last tag, that means the
              ;; tagbody doesn't either.
              do (unless rest
                   (return-from compile-ast :no-return))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GO-AST

(defmethod compile-ast ((ast cleavir-ast:go-ast) inserter system)
  (declare (ignore system))
  (destructuring-bind (catch iblock cfunction)
      (go-info (cleavir-ast:tag-ast ast))
    (let ((function (function inserter)))
      (cond
        ((eq function cfunction)
         ;; local
         (terminate inserter (make-instance 'cleavir-bir:jump
                               :inputs () :outputs ()
                               :next (list iblock))))
        (t
         (setf (go-info catch) t)
         ;; nonlocal
         (insert-unwind inserter catch iblock)))))
  :no-return)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALL-AST

(defmethod compile-ast ((ast cleavir-ast:call-ast) inserter system)
  (with-compiled-ast (callee (cleavir-ast:callee-ast ast)
                             inserter system)
    (with-compiled-arguments (args (cleavir-ast:argument-asts ast)
                                   inserter system)
      (insert inserter (make-instance 'cleavir-bir:call
                         :attributes (cleavir-ast:attributes ast)
                         :transforms (cleavir-ast:transforms ast)
                         :inputs (list* (first callee)
                                        (mapcar #'first args)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTION-AST

(defmethod compile-ast ((ast cleavir-ast:function-ast) inserter system)
  (let* ((f (or (gethash ast *function-info*)
                (setf (gethash ast *function-info*)
                      (compile-function ast system))))
         (enclose (make-instance 'cleavir-bir:enclose :code f)))
    (setf (cleavir-bir:enclose f) enclose)
    (list (insert inserter enclose))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LEXICAL-BIND-AST

(defmethod compile-ast ((ast cleavir-ast:lexical-bind-ast) inserter system)
  (let ((rv (compile-ast (cleavir-ast:value-ast ast) inserter system)))
    (cond ((eq rv :no-return) rv)
          (t
           (let* ((var (bind-variable (cleavir-ast:lhs-ast ast) (cleavir-ast:ignore ast)))
                  (leti (make-instance 'cleavir-bir:leti
                          :inputs (adapt inserter rv '(:object))
                          :outputs (list var))))
             (adjoin-variable inserter var)
             (insert inserter leti)
             (setf (cleavir-bir:binder var) leti))
           ;; return no values
           ()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SETQ-AST

(defmethod compile-ast ((ast cleavir-ast:setq-ast) inserter system)
  (let ((var (find-variable (cleavir-ast:lhs-ast ast))))
    (cleavir-bir:record-variable-set var)
    (with-compiled-ast (rv (cleavir-ast:value-ast ast) inserter system)
      (insert inserter
              (make-instance 'cleavir-bir:writevar
                :inputs rv :outputs (list var)))
      (list (insert inserter (make-instance 'cleavir-bir:readvar
                               :inputs (list var)))))))

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
  (if (cleavir-ctype:values-subtypep (cleavir-bir:ctype linear-datum)
                                     asserted-type
                                     system)
      linear-datum
      (insert inserter (make-instance 'cleavir-bir:thei
                         :inputs (list linear-datum)
                         :asserted-type asserted-type
                         :type-check-function type-check-function))))

(defmethod compile-ast ((ast cleavir-ast:the-ast) inserter system)
  (let* ((inner (cleavir-ast:form-ast ast))
         (ctype (cleavir-ast:ctype ast))
         (type-check-function-ast (cleavir-ast:type-check-function-ast ast))
         (required (cleavir-ctype:values-required ctype system))
         (optional (cleavir-ctype:values-optional ctype system))
         (rest (cleavir-ctype:values-rest ctype system))
         (rv (compile-ast inner inserter system))
         (type-check-function
           (if (symbolp type-check-function-ast)
               type-check-function-ast
               (compile-function type-check-function-ast system))))
    (cond ((or (eq rv :no-return))
           :no-return)
          ((some (lambda (ctype)
                   (cleavir-ctype:bottom-p ctype system))
                 required)
           (terminate inserter (make-instance 'cleavir-bir:unreachable))
           :no-return)
          ((listp rv) ; several single values
           (if (null (rest rv)) ; single value
               (list (wrap-thei inserter
                                (first rv)
                                (if (null required)
                                    (if (null optional)
                                        rest
                                        (first optional))
                                    (first required))
                                type-check-function
                                system))
               ;; FIXME: this is not as good as splitting this up into individual THEIs,
               ;; but we don't have a good way to split up type-check-function.
               (wrap-thei inserter (first (adapt inserter rv :multiple-values))
                          ctype type-check-function system)))
          (t ; arbitrary values
           (wrap-thei inserter rv ctype type-check-function system)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DYNAMIC-ALLOCATION-AST

(defmethod compile-ast ((ast cleavir-ast:dynamic-allocation-ast)
                        inserter system)
  (compile-ast (cleavir-ast:form-ast ast) inserter system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO
;;; CONSTANT-SYMBOL-VALUE-AST
;;; SET-CONSTANT-SYMBOL-VALUE-AST
;;; CONSTANT-FDEFINITION-AST

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPEQ-AST

(defmethod compile-test-ast ((ast cleavir-ast:typeq-ast) inserter system)
  (with-compiled-ast (obj (cleavir-ast:form-ast ast) inserter system)
    (let* ((tspec (cleavir-ast:test-ctype ast))
           ;; FIXME: Will get weird with custom ctypes.
           (tspec-str (write-to-string tspec))
           (tblock (make-iblock inserter
                                :name (symbolicate
                                       '#:typeq- tspec-str '#:-then)))
           (eblock (make-iblock inserter
                                :name (symbolicate
                                       '#:typeq- tspec-str '#:-else)))
           (tq (make-instance 'cleavir-bir:typeq-test
                 :inputs obj
                 :test-ctype tspec)))
      (insert inserter tq)
      (terminate inserter (make-instance 'cleavir-bir:ifi
                            :inputs (list tq)
                            :next (list tblock eblock)))
      (list tblock eblock))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPEW-AST

(defmethod compile-test-ast ((ast cleavir-ast:typew-ast) inserter system)
  (let ((rv (compile-ast (cleavir-ast:form-ast ast) inserter system)))
    (when (eq rv :no-return) (return-from compile-test-ast rv))
    (let ((old-iblock (iblock inserter))
          (tblock (make-iblock inserter :name '#:typew-then))
          (eblock (make-iblock inserter :name '#:typew-else))
          (test-iblock (make-iblock inserter :name '#:typew-test)))
      (begin inserter test-iblock)
      (let ((testrv (compile-test-ast (cleavir-ast:test-ast ast)
                                      inserter system)))
        (proceed inserter old-iblock)
        (cond ((eq testrv :no-return)
               (terminate inserter (make-instance 'cleavir-bir:jump
                                     :inputs () :outputs ()
                                     :next (list test-iblock)))
               (return-from compile-test-ast testrv))
              (t
               (terminate inserter (make-instance 'cleavir-bir:typew
                                     :inputs (adapt inserter rv '(:object))
                                     :ctype (cleavir-ast:ctype ast)
                                     :next (list tblock eblock test-iblock)))))
        (destructuring-bind (realtblock realeblock) testrv
          (proceed inserter realtblock)
          (terminate inserter (make-instance 'cleavir-bir:choke
                                :next (list tblock)))
          (proceed inserter realeblock)
          (terminate inserter (make-instance 'cleavir-bir:choke
                                :next (list eblock)))))
      (list tblock eblock))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; THE-TYPEW-AST

(defmethod compile-ast ((ast cleavir-ast:the-typew-ast) inserter system)
  (with-compiled-ast (rv (cleavir-ast:form-ast ast) inserter system)
    (let ((then-iblock (make-iblock inserter :name '#:the-typew-then))
          (else-iblock (make-iblock inserter :name '#:the-typew-else)))
      (terminate inserter (make-instance 'cleavir-bir:typew
                            :inputs rv
                            :ctype (cleavir-ast:ctype ast)
                            :next (list then-iblock else-iblock then-iblock)))
      (begin inserter else-iblock)
      (compile-ast (cleavir-ast:else-ast ast) inserter system)
      (begin inserter then-iblock))
    ;; if the value of the-typew is used, we'd have to introduce a variable,
    ;; since the form's value is used twice (as an input to typew, and as the
    ;; result of the-typew). But this is unlikely with the basic usage of using
    ;; the-typew in concert with multiple-value-extract. So we punt and return
    ;; no values.
    ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UNREACHABLE-AST

(defmethod compile-ast ((ast cleavir-ast:unreachable-ast) inserter system)
  (declare (ignore system))
  (terminate inserter (make-instance 'cleavir-bir:unreachable))
  :no-return)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LEXICAL-AST

(defmethod compile-ast ((ast cleavir-ast:lexical-ast) inserter system)
  (declare (ignore system))
  (let ((var (find-variable ast)))
    ;; FIXME: We probably want to make a new AST class to distinguish between
    ;; these two cases more cleanly.
    (typecase var
      (cleavir-bir:argument
       (list var))
      (t
       (cleavir-bir:record-variable-ref var)
       (list (insert inserter
                     (make-instance 'cleavir-bir:readvar
                       :inputs (list var))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EQ-AST

(defmethod compile-test-ast ((ast cleavir-ast:eq-ast) inserter system)
  (with-compiled-asts (args ((cleavir-ast:arg1-ast ast)
                             (cleavir-ast:arg2-ast ast))
                            inserter system (:object :object))
    (let ((tblock (make-iblock inserter :name '#:eq-then))
          (eblock (make-iblock inserter :name '#:eq-else)))
      (let ((eq-test (make-instance 'cleavir-bir:eq-test :inputs args)))
        (insert inserter eq-test)
        (terminate inserter (make-instance 'cleavir-bir:ifi
                              :inputs (list eq-test) :next (list tblock eblock))))
      (list tblock eblock))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NEQ-AST

(defmethod compile-test-ast ((ast cleavir-ast:neq-ast) inserter system)
  (with-compiled-asts (args ((cleavir-ast:arg1-ast ast)
                             (cleavir-ast:arg2-ast ast))
                            inserter system (:object :object))
    (let ((tblock (make-iblock inserter :name '#:neq-then))
          (eblock (make-iblock inserter :name '#:neq-else)))
      (let ((eq-test (make-instance 'cleavir-bir:eq-test :inputs args)))
        (insert inserter eq-test)
        (terminate inserter (make-instance 'cleavir-bir:ifi
                              :inputs (list eq-test) :next (list eblock tblock))))
      (list tblock eblock))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CASE-AST

(defmethod compile-test-ast ((ast cleavir-ast:case-ast) inserter system)
  (with-compiled-ast (obj (cleavir-ast:arg-ast ast) inserter system)
    (let* ((comparees (cleavir-ast:comparees ast))
           (iblocks (loop repeat (1+ (length comparees))
                          collect (make-iblock inserter))))
      (terminate inserter (make-instance 'cleavir-bir:case
                            :inputs obj
                            :comparees comparees :next iblocks))
      iblocks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;

(defmethod compile-ast ((ast cleavir-ast:constant-ast) inserter system)
  (declare (ignore system))
  (list
   (insert inserter
           (cleavir-bir:make-constant-reference
            (cleavir-bir:constant-in-module (cleavir-ast:value ast)
                                            *current-module*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LOAD-TIME-VALUE-AST. Needs work.

(defmethod compile-ast ((ast cleavir-ast:load-time-value-ast) inserter system)
  (declare (ignore system))
  (let ((inst (make-instance 'cleavir-bir:load-time-value
                :form (cleavir-ast:form ast)
                :read-only-p (cleavir-ast:read-only-p ast))))
    (cleavir-set:nadjoinf (cleavir-bir:load-time-values *current-module*) inst)
    (list (insert inserter inst))))
