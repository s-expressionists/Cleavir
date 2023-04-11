;;;; The idea of meta-evaluation on BIR is that they are mostly
;;;; bottom-up and require no flow analysis, corresponding to
;;;; expression level optimizations and propagation on the original
;;;; expression tree. Loosely based on the design of ir1opt.lisp by
;;;; Rob MacLachlan in CMU CL.

(in-package #:cleavir-bir-transformations)

(defun meta-evaluate-module (client module)
  ;; Obviously this should actually be a worklist algorithm and not
  ;; just two or three passes. We repeat on the module level so that
  ;; types are more likely to get propagated interprocedurally.
  (dotimes (repeat 3)
    (declare (ignorable repeat))
    (bir:do-functions (function module)
      ;; This check is necessary because meta-evaluation might have deleted
      ;; the function during our iteration. KLUDGE?
      (when (set:presentp function (bir:functions module))
        (meta-evaluate-function client function)
        (bir:compute-iblock-flow-order function)))))

;;; Prove that LINEAR-DATUM is of type DERIVED-TYPE.
(defun derive-type-for-linear-datum (client linear-datum derived-type)
  (setf (bir:derived-type linear-datum)
        (ctype:values-conjoin client (bir:ctype linear-datum) derived-type)))
;;; Pass along an assertion that LINEAR-DATUM is of type ASSERTED-TYPE.
(defun assert-type-for-linear-datum (client linear-datum asserted-type)
  (setf (bir:asserted-type linear-datum)
        (ctype:values-conjoin client (bir:asserted-type linear-datum)
                              asserted-type)))

(defun derive-attributes (linear-datum new-attributes)
  (setf (bir:attributes linear-datum)
        ;; join due to contravariance
        (attributes:join-attributes
         (bir:attributes linear-datum) new-attributes)))

(defun derive-local-call-parameter-types (client function argstypes)
  (let* ((bottom (ctype:bottom client)))
    (bir:map-lambda-list
     (lambda (state item index)
       (case state
         ((:required)
          (let ((type bottom))
            (dolist (argstype argstypes)
              (setf type (ctype:disjoin/2 client type
                                          (ctype:nth-value client index argstype))))
            (setf (bir:derived-type item) (ctype:single-value client type))))
         ((&optional)
          (let ((type bottom)
                (suppliedp nil))
            (dolist (argstype argstypes)
              (let ((nreq (length (ctype:values-required client argstype)))
                    (atype (ctype:nth-value client index argstype)))
                (setf type (ctype:disjoin/2 client type atype))
                (cond ((< index nreq) (pushnew t suppliedp))
                      ((ctype:bottom-p client atype) (pushnew nil suppliedp))
                      (t (setf suppliedp '(nil t))))))
            (setf (bir:derived-type (first item)) (ctype:single-value client type)
                  (bir:derived-type (second item))
                  (ctype:single-value client
                                      (apply #'ctype:member client suppliedp)))))
         ((&rest)
          ;; Here INDEX will be the number of required parameters plus the
          ;; number of optional parameters, which is useful.
          (let ((subtypes nil) (top (ctype:top client)))
            (dolist (argstype argstypes)
              (let ((nreq (length (ctype:values-required client argstype)))
                    (nopt (length (ctype:values-optional client argstype)))
                    (rest (ctype:values-rest client argstype)))
                (cond ((< index nreq) (pushnew 'cons subtypes))
                      ((< index (+ nreq nopt))
                       (setf subtypes '(null cons)))
                      ((ctype:bottom-p client rest) (pushnew 'null subtypes))
                      (t (setf subtypes '(null cons))))))
            (setf (bir:derived-type item)
                  (ctype:single-value
                   client
                   (cond ((equal subtypes '(null cons))
                          (ctype:disjoin/2 client (ctype:member client nil)
                                           (ctype:cons client top top)))
                         ((equal subtypes '(null)) (ctype:member client nil))
                         ((equal subtypes '(cons)) (ctype:cons client top top)))))))
         ;; anything else is too complicated.
         ))
     (bir:lambda-list function))))

(defun derive-local-call-argument-types-aux (client local-calls)
  (let ((bottom (ctype:bottom client)))
    ;; One values type for each local call, describing the arguments
    ;; to that call.
    (set:mapset
     'list
     (lambda (local-call)
       (etypecase local-call
         (bir:local-call
          (ctype:values client
                        (loop for arg in (rest (bir:inputs local-call))
                              collect (ctype:primary client (bir:ctype arg)))
                        nil bottom))
         (bir:mv-local-call
          (append-input-types
           client
           (mapcar #'bir:ctype (rest (bir:inputs local-call)))))))
     local-calls)))

(defun derive-local-call-argument-types (client function local-calls)
  (derive-local-call-parameter-types
   client function
   (derive-local-call-argument-types-aux client local-calls)))

;;; Derive the type of the function arguments from the types of the
;;; arguments of its local calls.
(defun derive-function-argument-types (client function)
  (let ((local-calls (bir:local-calls function)))
    (if (or (bir:enclose function) (set:empty-set-p local-calls))
        ;; If there is an enclose, we can be called from pretty much anywhere,
        ;; so there's not much we can determine about the arguments. We can mark
        ;; &rest arguments as being lists, and everything as being single
        ;; values at least.
        ;; If there is no enclose and no local calls, we treat this as a top
        ;; level function which could again be called from anywhere.
        (let* ((top (ctype:top client))
               (svtop (ctype:single-value client top)))
          (bir:map-lambda-list
           (lambda (state item index)
             (declare (ignore index))
             (case state
               ((:required) (derive-type-for-linear-datum client item svtop))
               ((&optional)
                (derive-type-for-linear-datum client (first item) svtop)
                (derive-type-for-linear-datum client (second item) svtop))
               ((&rest)
                (derive-type-for-linear-datum
                 client item
                 ;; LIST is of course (or null cons)
                 (ctype:single-value
                  client
                  (ctype:disjoin/2 client (ctype:member client nil)
                                   (ctype:cons client top top)))))
               ((&key)
                (derive-type-for-linear-datum client (second item) svtop)
                (derive-type-for-linear-datum client (third item) svtop))))
           (bir:lambda-list function)))
        (derive-local-call-argument-types client function local-calls))))

(defun meta-evaluate-function (client function)
  (derive-function-argument-types client function)
  ;; The decision for what goes in the forward vs backward flow passes
  ;; has to do with whether the effects of the optimization are on
  ;; things that happen before vs after in the flow graph, and if that
  ;; doesn't matter, which order makes it more likely to fire, so we
  ;; can feed effects as much as possible in one pass.
  (bir:do-iblocks (iblock function)
    ;; Make sure to merge the successors as much as possible so we can
    ;; trigger more optimizations.
    (loop while (bir:merge-successor-if-possible iblock))
    (meta-evaluate-iblock client iblock))
  (bir:do-iblocks (iblock function :backward)
    (flush-dead-code client iblock)
    ;; These transformations apply on empty iblocks, so we try them only
    ;; after the dead code flush.
    (let ((end (bir:end iblock)))
      (typecase end
        (bir:ifi (or (eliminate-if-if end) (eliminate-degenerate-if end)))
        (bir:jump (bir:delete-iblock-if-empty iblock))))))

;;; Derive the types of any iblock inputs. We have to do this from
;;; scratch optimistically because we are disjoining the types of the
;;; definitions, instead of narrowing the types conservatively.
(defun compute-phi-type (client phi)
  (let ((type (ctype:values-bottom client)))
    (set:doset (inp (bir:phi-inputs phi) type)
      (setq type (ctype:values-disjoin client type (bir:ctype inp))))))

(defun derive-iblock-input-types (client iblock)
  (dolist (phi (bir:inputs iblock))
    (setf (bir:derived-type phi) (compute-phi-type client phi))))

(defun compute-phi-attributes (phi)
  (let ((attr t))
    (set:doset (inp (bir:phi-inputs phi) attr)
      (setq attr (attributes:meet-attributes attr (bir:attributes inp))))))

(defun derive-iblock-input-attributes (iblock)
  (dolist (phi (bir:inputs iblock))
    (setf (bir:attributes phi) (compute-phi-attributes phi))))

(defun meta-evaluate-iblock (client iblock)
  (derive-iblock-input-types client iblock)
  (derive-iblock-input-attributes iblock)
  (bir:do-iblock-instructions (instruction iblock)
    ;; We derive types first. The type derivations should be correct regardless
    ;; of whether a rewrite is on the way, and if we do things in this order we
    ;; can derive types through any amount of straight line code during a single
    ;; meta-evaluate pass, promoting flow.
    (derive-types client instruction)
    (meta-evaluate-instruction client instruction)))

(defgeneric maybe-flush-instruction (client instruction))

(defmethod maybe-flush-instruction (client (instruction bir:instruction))
  (declare (ignore client)))

(defmethod maybe-flush-instruction (client (instruction bir:readvar))
  (declare (ignore client))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction
    (client (instruction bir:constant-reference))
  (declare (ignore client))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction
    (client (instruction bir:constant-fdefinition))
  (declare (ignore client))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction (client (instruction bir:enclose))
  (declare (ignore client))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction (client (instruction bir:conditional-test))
  (declare (ignore client))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction (client (instruction bir:fixed-to-multiple))
  (declare (ignore client))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction (client (instruction bir:values-restore))
  (declare (ignore client))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))

(defmethod maybe-flush-instruction (client (instruction bir:thei))
  (declare (ignore client))
  (when (and (bir:unused-p (bir:output instruction))
             ;; If this doesn't represent a check, it can be deleted.
             (symbolp (bir:type-check-function instruction)))
    (bir:delete-instruction instruction)))

(defgeneric flushable-call-p (client call identity)
  (:method (client (call bir:abstract-call) identity)
    (declare (ignore client identity))
    nil))

(defmethod maybe-flush-instruction (client (instruction bir:abstract-call))
  (when (and (bir:unused-p (bir:output instruction))
             (let ((ids (attributes:identities (bir:attributes instruction))))
               (and (not (null ids))
                    (every (lambda (id) (flushable-call-p client instruction id))
                           ids))))
    (bir:delete-instruction instruction)))

(defmethod maybe-flush-instruction (client (instruction bir:primop))
  (declare (ignore client))
  (let ((outs (bir:outputs instruction)))
    (when (and (not (null outs))
               (bir:unused-p (first outs))
               (attributes:has-flag-p (bir:attributes instruction) :flushable))
      (bir:delete-instruction instruction))))

(defun delete-terminator1 (inst)
  (bir:replace-terminator
   (make-instance 'bir:jump
     :next (bir:next inst) :inputs () :outputs ()
     :origin (bir:origin inst) :policy (bir:policy inst))
   inst))

(defmethod maybe-flush-instruction (client (inst bir:values-save))
  (declare (ignore client))
  (when (bir:unused-p (bir:output inst))
    (delete-terminator1 inst)))
(defmethod maybe-flush-instruction (client (inst bir:values-collect))
  (declare (ignore client))
  (when (bir:unused-p (bir:output inst))
    (delete-terminator1 inst)))

(defun flush-dead-code (client iblock)
  (bir:do-iblock-instructions (instruction iblock :backward)
    (maybe-flush-instruction client instruction))
  (dolist (phi (bir:inputs iblock))
    (when (null (bir:use phi))
      (bir:delete-phi phi))))

(defgeneric meta-evaluate-instruction (client instruction))

(defgeneric derive-types (client instruction))

(defmethod meta-evaluate-instruction (client instruction)
  ;; Without particular knowledge, we have nothing to do.
  (declare (ignore client instruction)))

(defmethod derive-types (client instruction)
  (declare (ignore client instruction)))

;;; Fold the IFI if we can determine whether or not the test will
;;; evaluate to NIL.
(defun fold-ifi (client instruction)
  (let* ((in (bir:input instruction))
         (inct (ctype:primary client (bir:ctype in)))
         (next (bir:next instruction))
         (then (first next))
         (else (second next)))
    (multiple-value-bind (next dead)
        (cond ((ctype:disjointp client inct (ctype:member client nil))
               #+(or)
               (format t "folding ifi based on type ~a" (bir:ctype in))
               (values then else))
              ((ctype:subtypep client inct (ctype:member client nil))
               #+(or)
               (print "folding ifi based on type NULL")
               (values else then)))
      (when dead
        #+(or)
        (print "folding ifi instruction")
        (bir:replace-terminator
         (make-instance 'bir:jump
           :next (list next)
           :inputs '() :outputs '()
           :origin (bir:origin instruction)
           :policy (bir:policy instruction))
         instruction)
        ;; Try to delete the block if possible, so we can maybe
        ;; optimize more in this pass. Ultimately, the flow order will
        ;; be recomputed.
        (bir:maybe-delete-iblock dead)
        t))))

;;; Eliminate IF IF constructs. Does the equivalent of (IF
;;; (IF X Y Z) A B) => (IF X (IF Y A B) (IF Z A B)). The reason this
;;; is optimization is desirable is that control flow is simplified,
;;; and also the flow of values is simplified by eliminating a phi
;;; which can lead to further optimization.
(defun eliminate-if-if (instruction)
  (let* ((iblock (bir:iblock instruction))
         (phis (bir:inputs iblock))
         (test (bir:input instruction)))
    ;; An IFI is eligible for this optimization if it starts its block
    ;; (i.e. is the only instruction in the block) and tests the phi
    ;; which is the unique input to its block.
    (when (and (eq instruction (bir:start iblock))
               (null (rest phis))
               (eq test (first phis)))
      #+(or)
      (print "eliminating if-if!")
      ;; We duplicate the IFI and replace the terminators for every
      ;; predecessor.
      (let ((next (bir:next instruction))
            (origin (bir:origin instruction))
            (policy (bir:policy instruction))
            (predecessors (bir:predecessors iblock)))
        ;; If one of the predecessors is an unwind or not a jump,
        ;; don't replace it
        (set:doset (predecessor predecessors)
          (let ((end (bir:end predecessor)))
            (when (or (not (typep end 'bir:jump)) (bir:unwindp end))
              (return-from eliminate-if-if nil))))
        ;; Actual work
        (set:doset (predecessor predecessors)
          (let* ((end (bir:end predecessor))
                 (input (first (bir:inputs end))))
            (assert (not (bir:unwindp end))
                    ()
                    "Don't replace jumps with unwinding action!")
            (assert (and (null (rest (bir:outputs end)))
                         (eq (first (bir:outputs end)) test))
                    ()
                    "Jump/phi pair inconsistent.")
            (let ((ifi (make-instance 'bir:ifi
                         :next (copy-list next) :inputs (list input)
                         :origin origin :policy policy)))
              (bir:replace-terminator ifi end)))))
      ;; Now we clean up the original IFI block.
      (bir:delete-iblock iblock)
      t)))

;;; Eliminate an IF if the two successors of IF are the same.
(defun eliminate-degenerate-if (ifi)
  (let* ((next (bir:next ifi))
         (succ (first next)))
    (when (eq succ (second next))
      #+(or)
      (print "ifi same block -> jump optimization")
      (change-class ifi 'bir:jump :outputs () :inputs ()
        :next (list succ)))))

(defmethod meta-evaluate-instruction (client (instruction bir:ifi))
  (fold-ifi client instruction))

;; Replace COMPUTATION with a constant reference to value.
(defun replace-computation-by-constant-value (instruction value)
  (let* ((constant 
           (bir:constant-in-module value (bir:module (bir:function instruction))))
         (constant-reference
           (make-instance 'bir:constant-reference
             :inputs (list constant) :outputs (bir:outputs instruction)
             :origin (bir:origin instruction) :policy (bir:policy instruction))))
    (bir:insert-instruction-before constant-reference instruction)
    (bir:delete-instruction instruction)))

;; Try to constant fold an instruction on INPUTS by applying FOLDER on its
;; inputs.
(defun constant-fold-instruction (instruction inputs folder)
  (let ((definers (loop for inp in inputs
                        if (typep inp 'bir:output)
                          collect (bir:definition inp)
                        else do (return-from constant-fold-instruction nil))))
    (when (every (lambda (definer) (typep definer 'bir:constant-reference))
                 definers)
      (replace-computation-by-constant-value
       instruction
       (apply folder
              (mapcar (lambda (def) (bir:constant-value (bir:input def)))
                      definers)))
      t)))

;;; If there is only one input and it has type (values something &rest nil),
;;; there is no need for this instruction.
(defmethod meta-evaluate-instruction (client (inst bir:fixed-to-multiple))
  (let ((inputs (bir:inputs inst)))
    (when (= (length inputs) 1)
      (let* ((input (first inputs))
             (inty (bir:ctype input)))
        (when (and (ctype:bottom-p client (ctype:values-rest client inty))
                   (null (ctype:values-optional client inty))
                   (= (length (ctype:values-required client inty)) 1))
          (setf (bir:inputs inst) nil)
          (let ((out (bir:output inst)))
            (bir:replace-uses input out)
            (bir:delete-instruction inst)
            t))))))

(defmethod derive-types (client (instruction bir:fixed-to-multiple))
  (let ((inputs (bir:inputs instruction)))
    ;; FIXME/KLUDGE: For now we only pass attributes for the primary value.
    (when (= (length inputs) 1)
      (derive-attributes (bir:output instruction)
                         (bir:attributes (first inputs))))
    (assert-type-for-linear-datum
     client (bir:output instruction)
     (ctype:values
      client
      (loop for inp in inputs
            collect (ctype:primary client (bir:asserted-type inp)))
      nil (ctype:bottom client)))
    (derive-type-for-linear-datum
     client (bir:output instruction)
     (ctype:values
      client
      (loop for inp in inputs
            collect (ctype:primary client (bir:ctype inp)))
      nil (ctype:bottom client)))))

(defmethod meta-evaluate-instruction (client (instruction bir:eq-test))
  (let ((inputs (bir:inputs instruction)))
    (cond ((constant-fold-instruction instruction inputs #'eq))
          ;; Objects of different types are never EQ.
          ((ctype:disjointp
            client
            (ctype:primary client (bir:ctype (first inputs)))
            (ctype:primary client (bir:ctype (second inputs))))
           (replace-computation-by-constant-value instruction nil)
           t)
          ;; (ifi (eq-test nil x) a b) => (if x b a)
          ((ctype:subtypep
            client
            (ctype:primary client (bir:ctype (first inputs)))
            (ctype:member client nil))
           (let* ((in (second inputs))
                  (out (bir:output instruction))
                  (ifi (bir:use out)))
             (when (typep ifi 'bir:ifi)
               (setf (bir:inputs ifi) (list in)
                     (bir:next ifi) (reverse (bir:next ifi)))
               (bir:delete-instruction instruction)
               t)))
          ;; (ifi (eq-test x nil) a b) => (if x b a)
          ((ctype:subtypep
            client
            (ctype:primary client (bir:ctype (second inputs)))
            (ctype:member client nil))
           (let* ((in (first inputs))
                  (out (bir:output instruction))
                  (ifi (bir:use out)))
             (when (typep ifi 'bir:ifi)
               (setf (bir:inputs ifi) (list in)
                     (bir:next ifi) (reverse (bir:next ifi)))
               (bir:delete-instruction instruction)
               t))))))

(defgeneric generate-type-check-function (client module origin ctype)
  ;; If the client does not specialize this function, do not reduce
  ;; typeq in the declared-but-not-verified case.
  (:method (client (module bir:module) origin ctype)
    (declare (ignore client origin ctype))
    nil))

(defun insert-type-check-before (client ctype input inst)
  (let ((tcf (generate-type-check-function client
                                           (bir:module (bir:function inst))
                                           (bir:origin inst)
                                           ctype)))
    (if tcf
        (let* ((actype (bir:asserted-type input))
               (cctype (ctype:conjoin client ctype
                                      (ctype:primary client (bir:ctype input))))
               (out (make-instance 'bir:output
                      :asserted-type actype
                      :derived-type (ctype:single-value client cctype)
                      :attributes (bir:attributes input)
                      :name (bir:name input)))
               (thei (make-instance 'bir:thei
                       :inputs (list input) :outputs (list out)
                       :origin (bir:origin inst) :policy (bir:policy inst)
                       :asserted-type (ctype:single-value client ctype)
                       :type-check-function tcf)))
          (bir:insert-instruction-before thei inst)
          t)
        nil)))

(defmethod meta-evaluate-instruction
    (client (instruction bir:typeq-test))
  (let* ((input (bir:input instruction))
         (ctype (ctype:primary client (bir:ctype input)))
         (actype (ctype:primary client (bir:asserted-type input)))
         (test-ctype (bir:test-ctype instruction)))
    (cond ((ctype:subtypep client ctype test-ctype)
           (replace-computation-by-constant-value instruction t)
           t)
          ((ctype:disjointp client ctype test-ctype)
           (replace-computation-by-constant-value instruction nil)
           t)
          ((ctype:subtypep client actype test-ctype)
           (when (insert-type-check-before client test-ctype input instruction)
             (replace-computation-by-constant-value instruction t)
             t))
          ((ctype:disjointp client actype test-ctype)
           (when (insert-type-check-before client
                                           (ctype:negate client test-ctype)
                                           input instruction)
             (replace-computation-by-constant-value instruction nil)
             t)))))

(defmethod derive-types (client (instruction bir:constant-reference))
  (derive-type-for-linear-datum
   client (bir:output instruction)
   (ctype:single-value
    client
    (ctype:member client (bir:constant-value (bir:input instruction))))))

(defmethod derive-types (client (instruction bir:constant-fdefinition))
  ;; Derive that it's a FUNCTION.
  (derive-type-for-linear-datum
   client (bir:output instruction)
   (ctype:single-value client (ctype:function-top client))))

(defmethod derive-types (client (instruction bir:constant-symbol-value))
  (derive-type-for-linear-datum
   client (bir:output instruction)
   (ctype:single-value client (ctype:top client))))

;;; Local variable with one reader and one writer can be substituted
;;; away,
(defun substitute-single-read-variable-if-possible (client variable)
  (let ((readers (bir:readers variable)))
    (when (and (bir:immutablep variable) (= (set:size readers) 1))
      (let* ((binder (bir:binder variable))
             (reader (set:arb readers))
             (reader-out (bir:output reader)))
        (when (eq (bir:function binder) (bir:function reader))
          #+(or)
          (format t "~&meta-evaluate: substituting single read binding of ~a" variable)
          (let* ((input (bir:input binder))
                 (type (ctype:single-value
                        client
                        (ctype:primary client (bir:ctype input))))
                 (fout (make-instance 'bir:output
                         :asserted-type type
                         :derived-type type))
                 (ftm (make-instance 'bir:fixed-to-multiple
                        :inputs (list input) :outputs (list fout)
                        :origin (bir:origin reader)
                        :policy (bir:policy reader))))
            (bir:insert-instruction-before ftm reader)
            (bir:replace-uses fout reader-out))
          (bir:delete-instruction reader)
          t)))))

;;; Variable bound to constant can get propagated.
(defun constant-propagate-variable-if-possible (variable)
  (when (bir:immutablep variable)
    (let* ((writer (bir:binder variable))
           (input (bir:input writer)))
      (when (typep input 'bir:output) ; FIXME: should be SSA?
        (let ((def (bir:definition input)))
          ;; FIXME: Should really check for a constant type here instead.
          (typecase def
            (bir:constant-reference
             (let ((constant (bir:input def)))
               (set:doset (reader (bir:readers variable))
                 (change-class reader 'bir:constant-reference
                               :inputs (list constant)))
               #+(or)
               (format t "~&meta-evaluate: constant propagating ~a"
                       (bir:constant-value constant))
               (bir:delete-instruction writer)
               (bir:delete-instruction def))
             t)
            (t nil)))))))

;;; For a variable, we prove that the type of its readers is just the union of
;;; the types of its writers. As with PHI we have to recompute each time around.
;;; Also: variables are always exactly one value.
(defun compute-variable-type (client variable getter)
  (let ((type (ctype:bottom client)))
    (set:doset (writer (bir:writers variable) (ctype:single-value client type))
      (let* ((inp (bir:input writer))
             (ity (ctype:primary client (funcall getter inp))))
        (setq type (ctype:disjoin client type ity))))))
(defun derive-type-for-variable (client variable)
  (let* ((type (compute-variable-type client variable #'bir:ctype)))
      (set:doset (reader (bir:readers variable))
        (let ((out (bir:output reader)))
          (derive-type-for-linear-datum client out type)))))
(defun assert-type-for-variable (client variable)
  (let* ((type (compute-variable-type client variable #'bir:asserted-type)))
    (set:doset (reader (bir:readers variable))
      (let ((out (bir:output reader)))
        (assert-type-for-linear-datum client out type)))))

(defun derive-attributes-for-variable (variable)
  (when (bir:immutablep variable)
    (let ((attr (bir:attributes (bir:input (bir:binder variable)))))
      (set:doset (reader (bir:readers variable))
        (derive-attributes (bir:output reader) attr)))))

(defmethod meta-evaluate-instruction (client (instruction bir:leti))
  (let ((variable (bir:output instruction)))
    (when variable
      (or (substitute-single-read-variable-if-possible client variable)
          (constant-propagate-variable-if-possible variable)))))

(defmethod derive-types (client (instruction bir:leti))
  (let ((variable (bir:output instruction)))
    (when variable
      (derive-attributes-for-variable variable)
      (assert-type-for-variable client variable)
      (derive-type-for-variable client variable))))

(defmethod derive-types (client (instruction bir:returni))
  ;; Propagate the return type to local calls and enclose of the function.
  (let ((function (bir:function instruction))
        (return-type (bir:ctype (bir:input instruction)))
        (areturn-type (bir:asserted-type (bir:input instruction))))
    (set:doset (local-call (bir:local-calls function))
      (let ((out (bir:output local-call)))
        (assert-type-for-linear-datum client out areturn-type)
        (derive-type-for-linear-datum client out return-type)))))

(defmethod derive-types (client (instruction bir:enclose))
  (let ((ftype (ctype:single-value client (ctype:function-top client))))
    (derive-type-for-linear-datum client (bir:output instruction) ftype)))

;;; If the number of values to be saved is known, record that.
(defmethod meta-evaluate-instruction (client (instruction bir:values-save))
  (let ((ity (bir:ctype (bir:input instruction))))
    (cond ((and (null (ctype:values-optional client ity))
                (ctype:bottom-p client (ctype:values-rest client ity)))
           (change-class instruction 'bir:fixed-values-save
                         :nvalues (length
                                   (ctype:values-required client ity)))
           t))))

;;; If already transformed, don't use the above method.
(defmethod meta-evaluate-instruction (client (inst bir:fixed-values-save))
  (declare (ignore client)))

(defmethod derive-types (client (instruction bir:values-save))
  (assert-type-for-linear-datum client (bir:output instruction)
                                (bir:asserted-type (bir:input instruction)))
  (derive-type-for-linear-datum client (bir:output instruction)
                                (bir:ctype (bir:input instruction))))

(defmethod derive-types (client (instruction bir:values-restore))
  (assert-type-for-linear-datum client (bir:output instruction)
                                (bir:asserted-type (bir:input instruction)))
  (derive-type-for-linear-datum client (bir:output instruction)
                                (bir:ctype (bir:input instruction))))

(defun append-input-types (client types)
  (apply #'ctype:values-append client types))

(defmethod meta-evaluate-instruction (client (instruction bir:values-collect))
  ;; Remove any inputs that are exactly zero values.
  (flet ((zvp (datum)
           (let ((ct (bir:ctype datum)))
             (and (null (ctype:values-required client ct))
                  (null (ctype:values-optional client ct))
                  (ctype:bottom-p client (ctype:values-rest client ct))))))
    (let ((inputs (bir:inputs instruction)))
      (when (some #'zvp inputs)
        (setf (bir:inputs instruction) (remove-if #'zvp inputs))
        t))))

(defmethod derive-types (client (instruction bir:values-collect))
  (let ((inputs (bir:inputs instruction)))
    (assert-type-for-linear-datum
     client (bir:output instruction)
     (append-input-types client (mapcar #'bir:asserted-type inputs)))
    (derive-type-for-linear-datum
     client (bir:output instruction)
     (append-input-types client (mapcar #'bir:ctype inputs)))))

;;; Move a thei to earlier in the code.
;;; It is not clear if this is permissible in general; while the behavior
;;; of a program with a violated type declaration is undefined, users might
;;; not expect an error until at earliest when the declaration is, so we don't
;;; lift checks unless there is a (possibly unchecked) declaration.
;;; See the meta-evaluate-instruction method below.
;;; Anyway, so we try to move the thei as far back as possible given
;;; this restriction. We try the point just after the input is defined,
;;; and then if the definition is one of certain instructions we keep moving
;;; up. We don't move past other checks in order to avoid repeated shuffling.
;;; Also, on non-SSAs we don't do anything.
(defgeneric lift-thei (client input thei)
  (:method (client (input bir:datum) (thei bir:thei))
    (declare (ignore client))
    nil))

(defun lift-thei-after (client input thei inst)
  (let ((new-out (make-instance 'bir:output
                   :derived-type (ctype:values-conjoin
                                  client
                                  ;; This thei is a check, therefore
                                  (bir:asserted-type thei)
                                  (bir:ctype input))
                   :asserted-type (ctype:values-conjoin
                                   client
                                   (bir:asserted-type thei)
                                   (bir:asserted-type input))
                   :attributes (bir:attributes input)
                   :name (bir:name input))))
    (assert (typep inst '(not bir:terminator)))
    ;; Remove the old thei assignment
    (bir:replace-uses (bir:input thei) (bir:output thei))
    ;; Put in the new output
    (bir:replace-uses new-out input)
    (setf (bir:inputs thei) (list input) (bir:outputs thei) (list new-out))
    ;; Move
    (bir:move-instruction-after thei inst)
    t))
(defun lift-thei-before (client input thei inst)
  (let ((new-out (make-instance 'bir:output
                   :derived-type (ctype:values-conjoin
                                  client
                                  ;; This thei is a check, therefore
                                  (bir:asserted-type thei)
                                  (bir:ctype input))
                   :asserted-type (ctype:values-conjoin
                                   client
                                   (bir:asserted-type thei)
                                   (bir:asserted-type input))
                   :attributes (bir:attributes input)
                   :name (bir:name input))))
    ;; Remove the old thei assignment
    (bir:replace-uses (bir:input thei) (bir:output thei))
    ;; Put in the new output
    (bir:replace-uses new-out input)
    (setf (bir:inputs thei) (list input) (bir:outputs thei) (list new-out))
    ;; Move
    (bir:move-instruction-before thei inst)
    t))
(defun lift-thei-to-iblock-start (client input thei iblock)
  ;; In order to avoid repeated shuffling, we do not move THEIs up past other
  ;; THEIs, even for other data, etc.
  (when (not (eq (bir:iblock thei) iblock))
    (bir:do-iblock-instructions (inst iblock)
      (if (typep inst 'bir:thei)
          (when (eq inst thei)
            ;; thei is already at head of iblock and only has other THEIs in
            ;; front of it. So don't move.
            (return-from lift-thei-to-iblock-start nil))
          ;; Found a non-THEI, so we can do the move.
          (return-from lift-thei-to-iblock-start
            (lift-thei-before client input thei inst))))))

(defgeneric lift-thei-through-inst (client output thei inst)
  (:method (client (datum bir:output) (thei bir:thei) (inst bir:instruction))
    (lift-thei-after client datum thei inst)))

(defmethod lift-thei-through-inst (client (thei-input bir:output)
                                   (thei bir:thei) (inst bir:thei))
  (let ((tcf (bir:type-check-function inst)))
    (if (symbolp tcf)
        ;; This might be the thei providing a specific enough
        ;; type for us to do the lift, in which case we
        ;; shouldn't lift past it.
        (if (ctype:values-subtypep
             client
             (bir:asserted-type (bir:input inst))
             (bir:asserted-type thei))
            ;; We're good, continue.
            (lift-thei client (bir:input inst) thei)
            ;; Nope we're done.
            (call-next-method))
        ;; Don't move past other checks, in order to avoid
        ;; repeated shuffling or any weird ordering issues.
        ;; Also to avoid propagating too tight a type to the type check
        ;; function itself before we move it.
        (call-next-method))))

(defmethod lift-thei-through-inst (client (thei-input bir:output)
                                   (thei bir:thei) (inst bir:readvar))
  (let ((var (bir:input inst)))
    (if (bir:immutablep var)
        (let ((wvinput (bir:input (bir:binder var))))
          ;; This check probably isn't actually necessary
          ;; (because where else would a declaration come from?)
          ;; but I am not totally sure.
          (if (ctype:values-subtypep
               client
               (bir:asserted-type wvinput)
               (bir:asserted-type thei))
              ;; Lift past the readvar and leti.
              (lift-thei client wvinput thei)
              (call-next-method)))
        ;; For now, at least, don't duplicate checks as we'd need to do
        ;; with multiple writers.
        (call-next-method))))

;;; TODO: fixed-to-multiple, at least?

(defmethod lift-thei (client (input bir:output) (thei bir:thei))
  (lift-thei-through-inst client input thei (bir:definition input)))

(defmethod lift-thei (client (thei-input bir:phi) (thei bir:thei))
  ;; For a phi with multiple sources, we'd have to replicate the THEI.
  ;; We don't do that. The THEI is just moved to the start of the phi's iblock.
  ;; Maybe in the future it could be good though?
  (lift-thei-to-iblock-start client thei-input thei (bir:iblock thei-input)))

(defmethod lift-thei (client (thei-input bir:argument) (thei bir:thei))
  ;; TODO: For an argument to a function that is only called in one place
  ;; locally, we could move the thei out of the function.
  (lift-thei-to-iblock-start client thei-input thei
                             (bir:start (bir:function thei))))

(defun insert-unreachable-after (instruction)
  ;; Avoid redundant work
  (unless (typep (bir:successor instruction) 'bir:unreachable)
    (multiple-value-bind (before after) (bir:split-block-after instruction)
      (bir:replace-terminator (make-instance 'bir:unreachable)
                              (bir:end before))
      (bir:delete-iblock after))))

(defmethod meta-evaluate-instruction (client (instruction bir:thei))
  (let ((input (bir:input instruction))
        (tcf (bir:type-check-function instruction)))
    (cond
      ;; If the type assertion definitely fails, mark subsequent code
      ;; as unreachable.
      ((and (ctype:values-disjointp client (bir:ctype input)
                                    (bir:asserted-type instruction))
            ;; Don't do this for untrusted assertions.
            tcf)
       (insert-unreachable-after instruction)
       ;; We don't delete the THEI. This is so that a warning can be
       ;; issued after meta evaluate (and so, only once) by
       ;; the code in generate-type-checks. Also so that at runtime
       ;; a good error is issued if there is a type-check-function.
       t)
      ;; Remove THEI when its input's type is a subtype of the
      ;; THEI's asserted type.
      ((ctype:values-subtypep client
                              (bir:ctype input)
                              (bir:asserted-type instruction))
       (bir:delete-thei instruction)
       t)
      ;; Also remove THEI when it's not a check and its input's asserted type
      ;; is a subtype of the THEI's. Means this THEI is redundant.
      ((and (symbolp tcf)
            (ctype:values-subtypep
             client
             (bir:asserted-type input)
             (bir:asserted-type instruction)))
       (bir:delete-thei instruction)
       t)
      ;; If this is a check and the asserted type is a subtype of the
      ;; check's type, we can lift the check.
      ;; e.g. if X is asserted as an (unsigned-byte 8), and we have
      ;; a check for type FIXNUM, we can move that check up to the
      ;; declaration.
      ((and (not (symbolp tcf))
            (ctype:values-subtypep
             client
             (bir:asserted-type input)
             (bir:asserted-type instruction)))
       (lift-thei client input instruction)))))

(defmethod derive-types (client (instruction bir:thei))
  (derive-attributes (bir:output instruction)
                     (bir:attributes (bir:input instruction)))
  (let* ((type-check-function (bir:type-check-function instruction))
         (input (bir:input instruction))
         (ctype (bir:ctype input)))
    ;; Compute the type of the THEI itself.
    ;; For THEI, the type we use to make inferences is the intersection
    ;; of what the compiler has proven about the input and what is
    ;; explicitly asserted when we are trusting THEI or explicitly type
    ;; checking. However, when the type check is marked as neither,
    ;; that means the compiler has not yet proven that the asserted type
    ;; holds, and so it must return the type of the input. This gives us
    ;; freedom to trust or explicitly check the assertion as needed while
    ;; making this decision transparent to inference, and also type conflict
    ;; when the type is checked elsewhere.
    (derive-type-for-linear-datum
     client (bir:output instruction)
     (if (eq type-check-function nil)
         ctype
         (ctype:values-conjoin client (bir:asserted-type instruction) ctype)))
    ;; The asserted type can be propagated even if it's not checked.
    (assert-type-for-linear-datum
     client (bir:output instruction)
     (ctype:values-conjoin client (bir:asserted-type instruction)
                           (bir:asserted-type input)))
    ;; Propagate the type of the input into function.
    (unless (symbolp type-check-function)
      (derive-local-call-parameter-types client type-check-function
                                         (list ctype)))))

;; Clients can specialize this to perform specific transformations on
;; the IR for a call.
;; The "transform" object is some thing stored in a function's attributes,
;; and so is client-defined. The attributes system has more info.
;; Methods should return true if a transformation took place, and otherwise
;; return false.
(defgeneric transform-call (client transform call)
  (:method (client transform (call bir:abstract-call))
    (declare (ignore client transform))
    nil))

(defgeneric fold-call (client fold call arguments)
  (:method (client fold (call bir:abstract-call) arguments)
    (declare (ignore client fold arguments))
    nil))

;;; Given a non-values ctype, returns two values:
;;; The value of the constant type, or NIL if it's not constant
;;; A boolean that's true iff it is constant
;;; FIXME: Move to ctype?
(defun constant-type-value (client ct)
  (cond ((ctype:member-p client ct)
         (let ((membs (ctype:member-members client ct)))
           (if (= (length membs) 1)
               (values (elt membs 0) t)
               (values nil nil))))
        ((ctype:rangep client ct)
         (multiple-value-bind (low lxp) (ctype:range-low client ct)
           (multiple-value-bind (high hxp) (ctype:range-high client ct)
             (if (or lxp hxp (not low) (not high) (not (eql low high)))
                 (values nil nil)
                 (values low t)))))
        (t (values nil nil))))

(defun constant-arguments (client arguments)
  (loop for arg in arguments
        for ct = (ctype:primary client (bir:ctype arg))
        collect (multiple-value-bind (cvalue constantp)
                    (constant-type-value client ct)
                  (if constantp
                      cvalue
                      (return (values nil nil))))
          into rargs
        finally (return (values rargs t))))

(defun maybe-fold-call-1 (client fold arguments instruction)
  (multiple-value-call
      (lambda (validp &rest values)
        (cond
          ((not validp) nil)
          ;; Splice in the values.
          ;; If there's only one, this is simple.
          ;; Otherwise we need a fixed-to-multiple.
          ((= (length values) 1)
           (replace-computation-by-constant-value instruction (first values))
           t)
          (t (let* ((module (bir:module (bir:function instruction)))
                    (origin (bir:origin instruction))
                    (policy (bir:policy instruction))
                    (constants
                      (loop for rv in values
                            collect (bir:constant-in-module rv module)))
                    (couts
                      (loop for rv in values
                            for ct = (ctype:member client rv)
                            for vct = (ctype:single-value client ct)
                            collect (make-instance 'bir:output
                                      :asserted-type vct
                                      :derived-type vct)))
                    (ftm
                      (make-instance 'bir:fixed-to-multiple
                        :inputs couts :outputs (bir:outputs instruction)
                        :origin origin :policy policy)))
               (loop for constant in constants
                     for cout in couts
                     for cref = (make-instance 'bir:constant-reference
                                  :inputs (list constant) :outputs (list cout)
                                  :origin origin :policy policy)
                     do (bir:insert-instruction-before cref instruction))
               (bir:insert-instruction-before ftm instruction)
               (bir:delete-instruction instruction)
               t))))
    (fold-call client fold instruction arguments)))

(defun maybe-fold-call (client folds arguments instruction)
  (when (not (null folds))
    (multiple-value-bind (args validp) (constant-arguments client arguments)
      (when validp
        (loop for fold in folds
                thereis (maybe-fold-call-1 client fold args instruction))))))

(defmethod meta-evaluate-instruction (client (instruction bir:call))
  (let* ((attr (bir:attributes instruction))
         (identities (attributes:identities attr)))
    (or
     ;; Try all client constant folds in order.
     ;; Folding is always preferable to transformation, so we try it first.
     (maybe-fold-call client identities (rest (bir:inputs instruction))
                      instruction)
     ;; Try all client transforms in order.
     ;; If any return true, a change has been made.
     (some (lambda (identity) (transform-call client identity instruction))
           identities))))

(defmethod meta-evaluate-instruction (client (instruction bir:primop))
  (let* ((attr (bir:attributes instruction))
         (identities (attributes:identities attr)))
    (or
     (maybe-fold-call client identities (bir:inputs instruction)
                      instruction)
     (some (lambda (identity) (transform-call client identity instruction))
           identities))))

(defun constant-mv-arguments (client vct)
  (if (and (null (ctype:values-optional client vct))
           (ctype:bottom-p client (ctype:values-rest client vct)))
      (loop for ct in (ctype:values-required client vct)
            collect (multiple-value-bind (cvalue constantp)
                        (constant-type-value client ct)
                      (if constantp
                          cvalue
                          (return (values nil nil))))
              into rargs
            finally (return (values rargs t)))
      (values nil nil)))

(defun maybe-fold-mv-call (client folds input-type instruction)
  (when (not (null folds))
    (multiple-value-bind (args validp)
        (constant-mv-arguments client input-type)
      (when validp
        (loop for fold in folds
                thereis (maybe-fold-call-1 client fold args instruction))))))

;;; Reduce an mv-call to a normal call if all its inputs are single-valued.
(defun mv-call->call (client mv-call)
  (let* ((args (second (bir:inputs mv-call)))
         (argsdef (and (typep args 'bir:output)
                       (bir:definition args))))
    (when (typep argsdef 'bir:values-collect)
      (let ((vcin (bir:inputs argsdef)))
        (flet ((svp (datum) ; single-value-p
                 (and (typep datum 'bir:output) ; can't rewrite w/o this
                      (let ((ct (bir:ctype datum)))
                        (and (= (length (ctype:values-required client ct)) 1)
                             (null (ctype:values-optional client ct))
                             (ctype:bottom-p client (ctype:values-rest client ct)))))))
          (when (every #'svp vcin)
            ;; OK, we can rewrite.
            ;; First, delete the values-collect.
            (delete-terminator1 argsdef)
            ;; This allows us to delete the values-saves.
            (let ((real-args
                    (loop for i in vcin
                          for d = (bir:definition i)
                          when (typep d 'bir:values-save)
                            collect (bir:input d)
                            and do (delete-terminator1 d)
                          else collect i)))
              ;; and finally, change-class.
              (change-class mv-call 'bir:call
                            :inputs (list* (bir:callee mv-call) real-args))
              t)))))))

(defmethod meta-evaluate-instruction (client (instruction bir:mv-call))
  (let ((identities (attributes:identities (bir:attributes instruction))))
    (or
     (maybe-fold-mv-call client identities
                         (append-input-types
                          client
                          (mapcar #'bir:ctype (rest (bir:inputs instruction))))
                         instruction)
     (some (lambda (identity) (transform-call client identity instruction))
           identities)
     (mv-call->call client instruction))))

;;; Given a client, an instruction, its identity (e.g. function name),
;;; and the VALUES type representing the incoming arguments, return
;;; the type of the result.
;;; FIXME: This might warrant a more complex type system in order to
;;; allow combining information.
;;; For example, (if test #'+ #'-) could still be seen to return two
;;; floats if given a float.
(defgeneric derive-return-type (client instruction identity argstype))
(defmethod derive-return-type (client (inst bir:abstract-call)
                               identity argstype)
  (declare (ignore identity argstype))
  (ctype:values-top client))

(defmethod derive-return-type (client (inst bir:primop)
                               identity argstype)
  (declare (ignore identity argstype))
  (ctype:values-top client))

(defmethod derive-types (client (inst bir:call))
  (let ((identities (attributes:identities (bir:attributes inst))))
    (flet ((intype (reader)
             (ctype:values client
                           (loop for arg in (rest (bir:inputs inst))
                                 for ct = (funcall reader arg)
                                 collect (ctype:primary client ct))
                           nil (ctype:bottom client)))
           (compute (identity intype)
             (derive-return-type client inst identity intype)))
      (cond ((null identities))
          ((= (length identities) 1)
           (derive-type-for-linear-datum
            client (bir:output inst)
            (compute (first identities) (intype #'bir:ctype)))
           (assert-type-for-linear-datum
            client (bir:output inst)
            (compute (first identities) (intype #'bir:asserted-type))))
          (t
           (let ((dtypes
                   (loop for identity in identities
                         collect (compute identity (intype #'bir:ctype))))
                 (atypes
                   (loop for identity in identities
                         collect (compute identity (intype #'bir:asserted-type)))))
             (derive-type-for-linear-datum client (bir:output inst)
                                           (apply #'ctype:values-conjoin
                                                  client dtypes))
             (assert-type-for-linear-datum client (bir:output inst)
                                           (apply #'ctype:values-conjoin
                                                  client atypes))))))))

(defmethod derive-types (client (inst bir:mv-call))
  (let ((identities (attributes:identities (bir:attributes inst))))
    (flet ((intype (reader)
             (append-input-types client (mapcar reader (rest (bir:inputs inst))))))
    (cond ((null identities))
          ((= (length identities) 1)
           (derive-type-for-linear-datum
            client (bir:output inst)
            (derive-return-type client inst (first identities) (intype #'bir:ctype)))
           (assert-type-for-linear-datum
            client (bir:output inst)
            (derive-return-type client inst (first identities)
                                (intype #'bir:asserted-type))))
          (t
           (let ((dtypes
                   (loop with arg = (intype #'bir:ctype)
                         for identity in identities
                         collect (derive-return-type client inst identity arg)))
                 (atypes
                   (loop with arg = (intype #'bir:asserted-type)
                         for identity in identities
                         collect (derive-return-type client inst identity arg))))
             (derive-type-for-linear-datum client (bir:output inst)
                                           (apply #'ctype:values-conjoin
                                                  client dtypes))
             (derive-type-for-linear-datum client (bir:output inst)
                                           (apply #'ctype:values-conjoin
                                                  client atypes))))))))

(defmethod derive-types (client (inst bir:primop))
  ;; Only do this when there's exactly one output.
  ;; derive-return-type doesn't make much sense otherwise.
  (when (= (length (bir:outputs inst)) 1)
    (let ((identities (attributes:identities (bir:attributes inst))))
      (flet ((intype (reader)
               (ctype:values client
                             (loop for arg in (rest (bir:inputs inst))
                                   for ct = (funcall reader arg)
                                   collect (ctype:primary client ct))
                             nil (ctype:bottom client)))
             (compute (identity intype)
               (derive-return-type client inst identity intype)))
        (cond ((null identities))
              ((= (length identities) 1)
               (derive-type-for-linear-datum
                client (bir:output inst)
                (compute (first identities) (intype #'bir:ctype)))
               (assert-type-for-linear-datum
                client (bir:output inst)
                (compute (first identities) (intype #'bir:asserted-type))))
              (t
               (let ((dtypes
                       (loop for identity in identities
                             collect (compute identity (intype #'bir:ctype))))
                     (atypes
                       (loop for identity in identities
                             collect (compute identity (intype #'bir:asserted-type)))))
                 (derive-type-for-linear-datum client (bir:output inst)
                                               (apply #'ctype:values-conjoin
                                                      client dtypes))
                 (assert-type-for-linear-datum client (bir:output inst)
                                               (apply #'ctype:values-conjoin
                                                      client atypes)))))))))

(defmethod meta-evaluate-instruction (client (inst bir:abstract-local-call))
  (declare (ignore client))
  ;; If a local function doesn't return, mark subsequent code unreachable
  ;; (unless we have already done so)
  (when (null (bir:returni (bir:callee inst)))
    (insert-unreachable-after inst)
    t))
