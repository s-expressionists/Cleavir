;;;; The idea of meta-evaluation on BIR is that they are mostly
;;;; bottom-up and require no flow analysis, corresponding to
;;;; expression level optimizations and propagation on the original
;;;; expression tree. Loosely based on the design of ir1opt.lisp by
;;;; Rob MacLachlan in CMU CL.

(in-package #:cleavir-bir-transformations)

;; This keeps track of whether any types were derived on the previous pass, or
;; the code was changed in some manner. We want to keep making passes until we
;; reach a fixed point, and have derived all the types we possibly can.
(defparameter *changed-anything-on-last-pass* T)

;; Simple flag to indicate whether we're on the first pass of meta-evaluation.
(defparameter *is-first-pass* T)

(defun meta-evaluate-module (module system)
  ;; First pass to establish a baseline for type derivation
  (bir:do-functions (function module)
    (when (set:presentp function (bir:functions module))
      (meta-evaluate-function function system)
      (bir:compute-iblock-flow-order function)))

  (setq *is-first-pass* NIL)
  
  ;; Obviously this should actually be a worklist algorithm and not
  ;; just two or three passes. We repeat on the module level so that
  ;; types are more likely to get propagated interprocedurally.
  (loop while *changed-anything-on-last-pass*
	;; Make sure we stop looping if we don't derive any types in the
	;; current iteration        
	do (setq *changed-anything-on-last-pass* NIL)

           ;; debug
           (format t "in main loop, about to evaluate the functions~%")
           
	   (bir:do-functions (function module)
	     ;; This check is necessary because meta-evaluation might have deleted
	     ;; the function during our iteration. KLUDGE?
	     (when (set:presentp function (bir:functions module))
               (meta-evaluate-function function system)
               (bir:compute-iblock-flow-order function)))))


;;; Given a linear datum, flag the instruction where it's used, along with the
;;; iblock and function containing the instruction.
(defun flag-datum-use (linear-datum)
  ;; flag the instruction using this datum for reprocessing
  (setf (:should-process (bir:use linear-datum)) T)
  ;; flag the instruction's iblock
  (setf (:should-process (bir:iblock (bir:use linear-datum))) T)
  ;; flag the iblock's function
  (setf (:should-process (bir:function (bir:iblock (bir:use linear-datum)))) T))

;;; Given an iblock, flag all of its instructions for reprocessing.
(defun flag-instructions-in-iblock (iblock)
  (bir:do-iblock-instructions (instruction iblock)
    (setf (:should-process instruction) T)))


;;; Prove that LINEAR-DATUM is of type DERIVED-TYPE.
;;; Returns T if a type was derived, and NIL otherwise.
(defun derive-type-for-linear-datum (linear-datum derived-type system)
  (let ((old-type (bir:ctype linear-datum)))
    (setf (bir:derived-type linear-datum)
          (ctype:values-conjoin system (bir:ctype linear-datum) derived-type))

    ;; If we derive a type in the current pass
    (when (and (not (equal old-type (bir:ctype linear-datum))) (cleavir-ctype:subtypep (bir:ctype linear-datum) old-type system) (bir:use linear-datum))
      (setq *changed-anything-on-last-pass* T)
      (flag-datum-use linear-datum)
;;      (format t "derived a type for this datum!~%")
      (return-from derive-type-for-linear-datum T)))
  (return-from derive-type-for-linear-datum NIL))

;;; Pass along an assertion that LINEAR-DATUM is of type ASSERTED-TYPE.
(defun assert-type-for-linear-datum (linear-datum asserted-type system)
  (setf (bir:asserted-type linear-datum)
        (ctype:values-conjoin system (bir:asserted-type linear-datum)
                              asserted-type)))

(defun derive-attributes (linear-datum new-attributes)
  (setf (bir:attributes linear-datum)
        ;; join due to contravariance
        (attributes:join-attributes
         (bir:attributes linear-datum) new-attributes)))

(defun derive-local-call-parameter-types (function argstypes system)
  (let* ((bottom (ctype:bottom system)))
    (bir:map-lambda-list
     (lambda (state item index)
       (case state
         ((:required)
          (let ((type bottom))
            (dolist (argstype argstypes)
              (setf type (ctype:disjoin/2 type
                                          (ctype:nth-value index argstype system)
                                          system)))
            (setf (bir:derived-type item) (ctype:single-value type system))))
         ((&optional)
          (let ((type bottom)
                (suppliedp nil))
            (dolist (argstype argstypes)
              (let ((nreq (length (ctype:values-required argstype system)))
                    (atype (ctype:nth-value index argstype system)))
                (setf type (ctype:disjoin/2 type atype system))
                (cond ((< index nreq) (pushnew t suppliedp))
                      ((ctype:bottom-p atype system) (pushnew nil suppliedp))
                      (t (setf suppliedp '(nil t))))))
            (setf (bir:derived-type (first item)) (ctype:single-value type system)
                  (bir:derived-type (second item))
                  (ctype:single-value (apply #'ctype:member system suppliedp)
                                      system))))
         ((&rest)
          ;; Here INDEX will be the number of required parameters plus the
          ;; number of optional parameters, which is useful.
          (let ((subtypes nil) (top (ctype:top system)))
            (dolist (argstype argstypes)
              (let ((nreq (length (ctype:values-required argstype system)))
                    (nopt (length (ctype:values-optional argstype system)))
                    (rest (ctype:values-rest argstype system)))
                (cond ((< index nreq) (pushnew 'cons subtypes))
                      ((< index (+ nreq nopt))
                       (setf subtypes '(null cons)))
                      ((ctype:bottom-p rest system) (pushnew 'null subtypes))
                      (t (setf subtypes '(null cons))))))
            (setf (bir:derived-type item)
                  (ctype:single-value
                   (cond ((equal subtypes '(null cons))
                          (ctype:disjoin/2 (ctype:member system nil)
                                           (ctype:cons top top system)
                                           system))
                         ((equal subtypes '(null)) (ctype:member system nil))
                         ((equal subtypes '(cons)) (ctype:cons top top system)))
                   system))))
         ;; anything else is too complicated.
         ))
     (bir:lambda-list function))))

(defun derive-local-call-argument-types-aux (local-calls system)
  (let ((bottom (ctype:bottom system)))
    ;; One values type for each local call, describing the arguments
    ;; to that call.
    (set:mapset
     'list
     (lambda (local-call)
       (etypecase local-call
         (bir:local-call
          (ctype:values (loop for arg in (rest (bir:inputs local-call))
                              collect (ctype:primary (bir:ctype arg) system))
                        nil bottom system))
         (bir:mv-local-call
          (append-input-types
           (mapcar #'bir:ctype (rest (bir:inputs local-call)))
           system))))
     local-calls)))

(defun derive-local-call-argument-types (function local-calls system)
  (derive-local-call-parameter-types
   function
   (derive-local-call-argument-types-aux local-calls system)
   system))

;;; Derive the type of the function arguments from the types of the
;;; arguments of its local calls.
(defun derive-function-argument-types (function system)
  (let ((local-calls (bir:local-calls function)))
    (if (or (bir:enclose function) (set:empty-set-p local-calls))
        ;; If there is an enclose, we can be called from pretty much anywhere,
        ;; so there's not much we can determine about the arguments. We can mark
        ;; &rest arguments as being lists, and everything as being single
        ;; values at least.
        ;; If there is no enclose and no local calls, we treat this as a top
        ;; level function which could again be called from anywhere.
        (let* ((top (ctype:top system))
               (svtop (ctype:single-value top system)))
          (bir:map-lambda-list
           (lambda (state item index)
             (declare (ignore index))
             (case state
               ((:required) (derive-type-for-linear-datum item svtop system))
               ((&optional)
                (derive-type-for-linear-datum (first item) svtop system)
                (derive-type-for-linear-datum (second item) svtop system))
               ((&rest)
                (derive-type-for-linear-datum
                 item
                 ;; LIST is of course (or null cons)
                 (ctype:single-value
                  (ctype:disjoin/2
                   (ctype:member system nil)
                   (ctype:cons top top system)
                   system)
                  system)
                 system))
               ((&key)
                (derive-type-for-linear-datum (second item) svtop system)
                (derive-type-for-linear-datum (third item) svtop system))))
           (bir:lambda-list function)))
        (derive-local-call-argument-types function local-calls system))))

(defun meta-evaluate-function (function system)
  (derive-function-argument-types function system)
  ;; The decision for what goes in the forward vs backward flow passes
  ;; has to do with whether the effects of the optimization are on
  ;; things that happen before vs after in the flow graph, and if that
  ;; doesn't matter, which order makes it more likely to fire, so we
  ;; can feed effects as much as possible in one pass.
  (bir:do-iblocks (iblock function)
    ;; Make sure to merge the successors as much as possible so we can
    ;; trigger more optimizations.
    (loop while (bir:merge-successor-if-possible iblock))
    (meta-evaluate-iblock iblock system))
  (bir:do-iblocks (iblock function :backward)
    (flush-dead-code iblock system)
    ;; These transformations apply on empty iblocks, so we try them only
    ;; after the dead code flush.
    (let ((end (bir:end iblock)))
      (typecase end
        (bir:ifi (or (eliminate-if-if end) (eliminate-degenerate-if end)))
        (bir:jump (bir:delete-iblock-if-empty iblock))))))

;;; Derive the types of any iblock inputs. We have to do this from
;;; scratch optimistically because we are disjoining the types of the
;;; definitions, instead of narrowing the types conservatively.
(defun compute-phi-type (phi system)
  (let ((type (ctype:values-bottom system)))
    (set:doset (inp (bir:phi-inputs phi) type)
      (setq type (ctype:values-disjoin system type (bir:ctype inp))))))

(defun derive-iblock-input-types (iblock system)
  (dolist (phi (bir:inputs iblock))
    (setf (bir:derived-type phi) (compute-phi-type phi system))))

(defun compute-phi-attributes (phi)
  (let ((attr t))
    (set:doset (inp (bir:phi-inputs phi) attr)
      (setq attr (attributes:meet-attributes attr (bir:attributes inp))))))

(defun derive-iblock-input-attributes (iblock)
  (dolist (phi (bir:inputs iblock))
    (setf (bir:attributes phi) (compute-phi-attributes phi))))

(defun meta-evaluate-iblock (iblock system)
  (derive-iblock-input-types iblock system)
  (derive-iblock-input-attributes iblock)
  (bir:do-iblock-instructions (instruction iblock)
    ;; We derive types first. The type derivations should be correct regardless
    ;; of whether a rewrite is on the way, and if we do things in this order we
    ;; can derive types through any amount of straight line code during a single
    ;; meta-evaluate pass, promoting flow.
    (when (:should-process instruction)
      (derive-types instruction system)
      ;; Since meta-evaluate-instruction can transform the code, we need to
      ;; make sure that we process any new code that might appear.
      (when (meta-evaluate-instruction instruction system)
        (setq *changed-anything-on-last-pass* T)
        ;; debug
;;        (format t "Just set *changed-anything-on-last-pass* to T~%")
;;        (flag-instructions-in-iblock iblock)
        ))))

(defgeneric maybe-flush-instruction (instruction system))

(defmethod maybe-flush-instruction ((instruction bir:instruction) system)
  (declare (ignore system)))

(defmethod maybe-flush-instruction ((instruction bir:readvar) system)
  (declare (ignore system))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction
    ((instruction bir:constant-reference) system)
  (declare (ignore system))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction
    ((instruction bir:constant-fdefinition) system)
  (declare (ignore system))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction ((instruction bir:enclose) system)
  (declare (ignore system))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction ((instruction bir:conditional-test) system)
  (declare (ignore system))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction ((instruction bir:fixed-to-multiple) system)
  (declare (ignore system))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction ((instruction bir:values-restore) system)
  (declare (ignore system))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))

(defmethod maybe-flush-instruction ((instruction bir:thei) system)
  (declare (ignore system))
  (when (and (bir:unused-p (bir:output instruction))
             ;; If this doesn't represent a check, it can be deleted.
             (symbolp (bir:type-check-function instruction)))
    (bir:delete-instruction instruction)))

(defgeneric flushable-call-p (call identity system)
  (:method ((call bir:abstract-call) identity system)
    (declare (ignore identity system))
    nil))

(defmethod maybe-flush-instruction ((instruction bir:abstract-call) system)
  (when (and (bir:unused-p (bir:output instruction))
             (let ((ids (attributes:identities (bir:attributes instruction))))
               (and (not (null ids))
                    (every (lambda (id) (flushable-call-p instruction id system))
                           ids))))
    (bir:delete-instruction instruction)))

(defmethod maybe-flush-instruction ((instruction bir:primop) system)
  (declare (ignore system))
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

(defmethod maybe-flush-instruction ((inst bir:values-save) system)
  (declare (ignore system))
  (when (bir:unused-p (bir:output inst))
    (delete-terminator1 inst)))
(defmethod maybe-flush-instruction ((inst bir:values-collect) system)
  (declare (ignore system))
  (when (bir:unused-p (bir:output inst))
    (delete-terminator1 inst)))

(defun flush-dead-code (iblock system)
  (bir:do-iblock-instructions (instruction iblock :backward)
    (maybe-flush-instruction instruction system))
  (dolist (phi (bir:inputs iblock))
    (when (null (bir:use phi))
      (bir:delete-phi phi))))

(defgeneric meta-evaluate-instruction (instruction system))

(defgeneric derive-types (instruction system))

(defmethod meta-evaluate-instruction (instruction system)
  ;; Without particular knowledge, we have nothing to do.
  (declare (ignore instruction system)))

(defmethod derive-types (instruction system)
  (declare (ignore instruction system)))

;;; Fold the IFI if we can determine whether or not the test will
;;; evaluate to NIL.
(defun fold-ifi (instruction system)
  (let* ((in (bir:input instruction))
         (inct (ctype:primary (bir:ctype in) system))
         (next (bir:next instruction))
         (then (first next))
         (else (second next)))
    (multiple-value-bind (next dead)
        (cond ((ctype:disjointp inct (ctype:member system nil) system)
               #+(or)
               (format t "folding ifi based on type ~a" (bir:ctype in))
               (values then else))
              ((ctype:subtypep inct (ctype:member system nil) system)
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

(defmethod meta-evaluate-instruction ((instruction bir:ifi) system)
  (fold-ifi instruction system))

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
(defmethod meta-evaluate-instruction ((inst bir:fixed-to-multiple)
                                      system)
  (let ((inputs (bir:inputs inst)))
    (when (= (length inputs) 1)
      (let* ((input (first inputs))
             (inty (bir:ctype input)))
        (when (and (ctype:bottom-p (ctype:values-rest inty system) system)
                   (null (ctype:values-optional inty system))
                   (= (length (ctype:values-required inty system)) 1))
          (setf (bir:inputs inst) nil)
          (let ((out (bir:output inst)))
            (bir:replace-uses input out)
            (bir:delete-instruction inst)
            t))))))

(defmethod derive-types ((instruction bir:fixed-to-multiple) system)
  (let ((inputs (bir:inputs instruction)))
    ;; FIXME/KLUDGE: For now we only pass attributes for the primary value.
    (when (= (length inputs) 1)
      (derive-attributes (bir:output instruction)
                         (bir:attributes (first inputs))))
    (assert-type-for-linear-datum
     (bir:output instruction)
     (ctype:values
      (loop for inp in inputs
            collect (ctype:primary (bir:asserted-type inp) system))
      nil
      (ctype:bottom system)
      system)
     system)
    (let ((derived-a-type (derive-type-for-linear-datum
     (bir:output instruction)
     (ctype:values
      (loop for inp in inputs
            collect (ctype:primary (bir:ctype inp) system))
      nil
      (ctype:bottom system)
      system)
     system)))
      ;; If we didn't derive a type for this datum, and we've analyzed the
      ;; instruction before, then we can ignore it for now.
      (when (and (not derived-a-type) (not *is-first-pass*))
        (setf (:should-process instruction) NIL)))))

(defmethod meta-evaluate-instruction ((instruction bir:eq-test) system)
  (let ((inputs (bir:inputs instruction)))
    (cond ((constant-fold-instruction instruction inputs #'eq))
          ;; Objects of different types are never EQ.
          ((ctype:disjointp
            (ctype:primary (bir:ctype (first inputs)) system)
            (ctype:primary (bir:ctype (second inputs)) system)
            system)
           (replace-computation-by-constant-value instruction nil)
           t)
          ;; (ifi (eq-test nil x) a b) => (if x b a)
          ((ctype:subtypep
            (ctype:primary (bir:ctype (first inputs)) system)
            (ctype:member system nil)
            system)
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
            (ctype:primary (bir:ctype (second inputs)) system)
            (ctype:member system nil)
            system)
           (let* ((in (first inputs))
                  (out (bir:output instruction))
                  (ifi (bir:use out)))
             (when (typep ifi 'bir:ifi)
               (setf (bir:inputs ifi) (list in)
                     (bir:next ifi) (reverse (bir:next ifi)))
               (bir:delete-instruction instruction)
               t))))))

(defgeneric generate-type-check-function (module origin ctype system)
  ;; If the client does not specialize this function, do not reduce
  ;; typeq in the declared-but-not-verified case.
  (:method ((module bir:module) origin ctype system)
    (declare (ignore origin ctype system))
    nil))

(defun insert-type-check-before (ctype input inst system)
  (let ((tcf (generate-type-check-function (bir:module (bir:function inst))
                                           (bir:origin inst) ctype system)))
    (if tcf
        (let* ((actype (bir:asserted-type input))
               (cctype (ctype:conjoin system ctype
                                      (ctype:primary (bir:ctype input) system)))
               (out (make-instance 'bir:output
                      :asserted-type actype
                      :derived-type (ctype:single-value cctype system)
                      :attributes (bir:attributes input)
                      :name (bir:name input)))
               (thei (make-instance 'bir:thei
                       :inputs (list input) :outputs (list out)
                       :origin (bir:origin inst) :policy (bir:policy inst)
                       :asserted-type (ctype:single-value ctype system)
                       :type-check-function tcf)))
          (bir:insert-instruction-before thei inst)
          t)
        nil)))

(defmethod meta-evaluate-instruction
    ((instruction bir:typeq-test) system)
  (let* ((input (bir:input instruction))
         (ctype (ctype:primary (bir:ctype input) system))
         (actype (ctype:primary (bir:asserted-type input) system))
         (test-ctype (bir:test-ctype instruction)))
    (cond ((ctype:subtypep ctype test-ctype system)
           (replace-computation-by-constant-value instruction t)
           t)
          ((ctype:disjointp ctype test-ctype system)
           (replace-computation-by-constant-value instruction nil)
           t)
          ((ctype:subtypep actype test-ctype system)
           (when (insert-type-check-before test-ctype input instruction system)
             (replace-computation-by-constant-value instruction t)
             t))
          ((ctype:disjointp actype test-ctype system)
           (when (insert-type-check-before (ctype:negate test-ctype system)
                                           input instruction system)
             (replace-computation-by-constant-value instruction nil)
             t)))))

(defmethod derive-types ((instruction bir:constant-reference) system)
  (let ((derived-a-type (derive-type-for-linear-datum
   (bir:output instruction)
   (ctype:single-value
    (ctype:member system (bir:constant-value (bir:input instruction)))
    system)
   system)))
    ;; If we didn't derive a type for this datum, and we've analyzed the
    ;; instruction before, then we can ignore it for now.
    (when (and (not derived-a-type) (not *is-first-pass*))
      (setf (:should-process instruction) NIL))))

(defmethod derive-types ((instruction bir:constant-fdefinition) system)
  ;; Derive that it's a FUNCTION.
  (let ((derived-a-type (derive-type-for-linear-datum
   (bir:output instruction)
   (ctype:single-value (ctype:function-top system) system)
   system)))
     ;; If we didn't derive a type for this datum, and we've analyzed the
     ;; instruction before, then we can ignore it for now.
     (when (and (not derived-a-type) (not *is-first-pass*))
       (setf (:should-process instruction) NIL))))
     

(defmethod derive-types ((instruction bir:constant-symbol-value) system)
  (let ((derived-a-type (derive-type-for-linear-datum
   (bir:output instruction)
   (ctype:single-value (ctype:top system) system)
   system)))
    ;; If we didn't derive a type for this datum, and we've analyzed the
    ;; instruction before, then we can ignore it for now.
    (when (and (not derived-a-type) (not *is-first-pass*))
      (setf (:should-process instruction) NIL))))

;;; Local variable with one reader and one writer can be substituted
;;; away,
(defun substitute-single-read-variable-if-possible (variable system)
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
                        (ctype:primary (bir:ctype input) system)
                        system))
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
(defun compute-variable-type (variable getter system)
  (let ((type (ctype:bottom system)))
    (set:doset (writer (bir:writers variable) (ctype:single-value type system))
      (let* ((inp (bir:input writer))
             (ity (ctype:primary (funcall getter inp) system)))
        (setq type (ctype:disjoin system type ity))))))
(defun derive-type-for-variable (variable system)
  (let* ((type (compute-variable-type variable #'bir:ctype system)))
      (set:doset (reader (bir:readers variable))
        (let ((out (bir:output reader)))
          (derive-type-for-linear-datum out type system)))))
(defun assert-type-for-variable (variable system)
  (let* ((type (compute-variable-type variable #'bir:asserted-type system)))
    (set:doset (reader (bir:readers variable))
      (let ((out (bir:output reader)))
        (assert-type-for-linear-datum out type system)))))

(defun derive-attributes-for-variable (variable)
  (when (bir:immutablep variable)
    (let ((attr (bir:attributes (bir:input (bir:binder variable)))))
      (set:doset (reader (bir:readers variable))
        (derive-attributes (bir:output reader) attr)))))

(defmethod meta-evaluate-instruction ((instruction bir:leti) system)
  (let ((variable (bir:output instruction)))
    (when variable
      (or (substitute-single-read-variable-if-possible variable system)
          (constant-propagate-variable-if-possible variable)))))

(defmethod derive-types ((instruction bir:leti) system)
  (let ((variable (bir:output instruction)))
    (when variable
      (derive-attributes-for-variable variable)
      (assert-type-for-variable variable system)
      (derive-type-for-variable variable system))))

(defmethod derive-types ((instruction bir:returni) system)
  ;; Propagate the return type to local calls and enclose of the function.
  (let ((function (bir:function instruction))
        (return-type (bir:ctype (bir:input instruction)))
        (areturn-type (bir:asserted-type (bir:input instruction))))
    (set:doset (local-call (bir:local-calls function))
      (let ((out (bir:output local-call)))
        (assert-type-for-linear-datum out areturn-type system)
        (let ((derived-a-type (derive-type-for-linear-datum out return-type system)))
          ;; If we didn't derive a type for this datum, and we've analyzed the
          ;; instruction before, then we can ignore it for now.
          (when (and (not derived-a-type) (not *is-first-pass*))
            (setf (:should-process instruction) NIL)))))))

(defmethod derive-types ((instruction bir:enclose) system)
  (let ((ftype (ctype:single-value (ctype:function-top system) system)))
    (let ((derived-a-type (derive-type-for-linear-datum (bir:output instruction) ftype system)))
      ;; If we didn't derive a type for this datum, and we've analyzed the
      ;; instruction before, then we can ignore it for now.
      (when (and (not derived-a-type) (not *is-first-pass*))
        (setf (:should-process instruction) NIL)))))

;;; If the number of values to be saved is known, record that.
(defmethod meta-evaluate-instruction ((instruction bir:values-save) system)
  (let ((ity (bir:ctype (bir:input instruction))))
    (cond ((and (null (ctype:values-optional ity system))
                (ctype:bottom-p (ctype:values-rest ity system) system))
           (change-class instruction 'bir:fixed-values-save
                         :nvalues (length
                                   (ctype:values-required ity system)))
           t))))

;;; If already transformed, don't use the above method.
(defmethod meta-evaluate-instruction ((inst bir:fixed-values-save) system)
  (declare (ignore system)))

(defmethod derive-types ((instruction bir:values-save) system)
  (assert-type-for-linear-datum (bir:output instruction)
                                (bir:asserted-type (bir:input instruction))
                                system)
  (let ((derived-a-type (derive-type-for-linear-datum (bir:output instruction)
                                (bir:ctype (bir:input instruction))
                                system)))
    ;; If we didn't derive a type for this datum, and we've analyzed the
    ;; instruction before, then we can ignore it for now.
    (when (and (not derived-a-type) (not *is-first-pass*))
      (setf (:should-process instruction) NIL))))

(defmethod derive-types ((instruction bir:values-restore) system)
  (assert-type-for-linear-datum (bir:output instruction)
                                (bir:asserted-type (bir:input instruction))
                                system)
  (let ((derived-a-type (derive-type-for-linear-datum (bir:output instruction)
                                (bir:ctype (bir:input instruction))
                                system)))
    ;; If we didn't derive a type for this datum, and we've analyzed the
    ;; instruction before, then we can ignore it for now.
    (when (and (not derived-a-type) (not *is-first-pass*))
      (setf (:should-process instruction) NIL))))

(defun append-input-types (types system)
  (apply #'ctype:values-append system types))

(defmethod meta-evaluate-instruction ((instruction bir:values-collect) system)
  ;; Remove any inputs that are exactly zero values.
  (flet ((zvp (datum)
           (let ((ct (bir:ctype datum)))
             (and (null (ctype:values-required ct system))
                  (null (ctype:values-optional ct system))
                  (ctype:bottom-p (ctype:values-rest ct system) system)))))
    (let ((inputs (bir:inputs instruction)))
      (when (some #'zvp inputs)
        (setf (bir:inputs instruction) (remove-if #'zvp inputs))
        t))))

(defmethod derive-types ((instruction bir:values-collect) system)
  (let ((inputs (bir:inputs instruction)))
    (assert-type-for-linear-datum
     (bir:output instruction)
     (append-input-types (mapcar #'bir:asserted-type inputs) system)
     system)
    (let ((derived-a-type (derive-type-for-linear-datum
     (bir:output instruction)
     (append-input-types (mapcar #'bir:ctype inputs) system)
     system)))
      ;; If we didn't derive a type for this datum, and we've analyzed the
      ;; instruction before, then we can ignore it for now.
      (when (and (not derived-a-type) (not *is-first-pass*))
        (setf (:should-process instruction) NIL)))))

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
(defgeneric lift-thei (input thei system)
  (:method ((input bir:datum) (thei bir:thei) system)
    (declare (ignore system))
    nil))

(defun lift-thei-after (input thei inst system)
  (let ((new-out (make-instance 'bir:output
                   :derived-type (ctype:values-conjoin
                                  system
                                  ;; This thei is a check, therefore
                                  (bir:asserted-type thei)
                                  (bir:ctype input))
                   :asserted-type (ctype:values-conjoin
                                   system
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
(defun lift-thei-before (input thei inst system)
  (let ((new-out (make-instance 'bir:output
                   :derived-type (ctype:values-conjoin
                                  system
                                  ;; This thei is a check, therefore
                                  (bir:asserted-type thei)
                                  (bir:ctype input))
                   :asserted-type (ctype:values-conjoin
                                   system
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
(defun lift-thei-to-iblock-start (input thei iblock system)
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
            (lift-thei-before input thei inst system))))))

(defgeneric lift-thei-through-inst (output thei inst system)
  (:method ((datum bir:output) (thei bir:thei) (inst bir:instruction) system)
    (lift-thei-after datum thei inst system)))

(defmethod lift-thei-through-inst ((thei-input bir:output) (thei bir:thei)
                                   (inst bir:thei) system)
  (let ((tcf (bir:type-check-function inst)))
    (if (symbolp tcf)
        ;; This might be the thei providing a specific enough
        ;; type for us to do the lift, in which case we
        ;; shouldn't lift past it.
        (if (ctype:values-subtypep
             (bir:asserted-type (bir:input inst))
             (bir:asserted-type thei)
             system)
            ;; We're good, continue.
            (lift-thei (bir:input inst) thei system)
            ;; Nope we're done.
            (call-next-method))
        ;; Don't move past other checks, in order to avoid
        ;; repeated shuffling or any weird ordering issues.
        ;; Also to avoid propagating too tight a type to the type check
        ;; function itself before we move it.
        (call-next-method))))

(defmethod lift-thei-through-inst ((thei-input bir:output) (thei bir:thei)
                                   (inst bir:readvar) system)
  (let ((var (bir:input inst)))
    (if (bir:immutablep var)
        (let ((wvinput (bir:input (bir:binder var))))
          ;; This check probably isn't actually necessary
          ;; (because where else would a declaration come from?)
          ;; but I am not totally sure.
          (if (ctype:values-subtypep
               (bir:asserted-type wvinput)
               (bir:asserted-type thei)
               system)
              ;; Lift past the readvar and leti.
              (lift-thei wvinput thei system)
              (call-next-method)))
        ;; For now, at least, don't duplicate checks as we'd need to do
        ;; with multiple writers.
        (call-next-method))))

;;; TODO: fixed-to-multiple, at least?

(defmethod lift-thei ((input bir:output) (thei bir:thei) system)
  (lift-thei-through-inst input thei (bir:definition input) system))

(defmethod lift-thei ((thei-input bir:phi) (thei bir:thei) system)
  ;; For a phi with multiple sources, we'd have to replicate the THEI.
  ;; We don't do that. The THEI is just moved to the start of the phi's iblock.
  ;; Maybe in the future it could be good though?
  (lift-thei-to-iblock-start thei-input thei (bir:iblock thei-input) system))

(defmethod lift-thei ((thei-input bir:argument) (thei bir:thei) system)
  ;; TODO: For an argument to a function that is only called in one place
  ;; locally, we could move the thei out of the function.
  (lift-thei-to-iblock-start thei-input thei (bir:start (bir:function thei))
                             system))

(defun insert-unreachable-after (instruction)
  ;; Avoid redundant work
  (unless (typep (bir:successor instruction) 'bir:unreachable)
    (multiple-value-bind (before after) (bir:split-block-after instruction)
      (bir:replace-terminator (make-instance 'bir:unreachable)
                              (bir:end before))
      (bir:delete-iblock after))))

(defmethod meta-evaluate-instruction ((instruction bir:thei) system)
  (let ((input (bir:input instruction))
        (tcf (bir:type-check-function instruction)))
    (cond
      ;; If the type assertion definitely fails, mark subsequent code
      ;; as unreachable.
      ((and (ctype:values-disjointp (bir:ctype input)
                                    (bir:asserted-type instruction)
                                    system)
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
      ((ctype:values-subtypep (bir:ctype input)
                              (bir:asserted-type instruction)
                              system)
       (bir:delete-thei instruction)
       t)
      ;; Also remove THEI when it's not a check and its input's asserted type
      ;; is a subtype of the THEI's. Means this THEI is redundant.
      ((and (symbolp tcf)
            (ctype:values-subtypep
             (bir:asserted-type input)
             (bir:asserted-type instruction)
             system))
       (bir:delete-thei instruction)
       t)
      ;; If this is a check and the asserted type is a subtype of the
      ;; check's type, we can lift the check.
      ;; e.g. if X is asserted as an (unsigned-byte 8), and we have
      ;; a check for type FIXNUM, we can move that check up to the
      ;; declaration.
      ((and (not (symbolp tcf))
            (ctype:values-subtypep
             (bir:asserted-type input)
             (bir:asserted-type instruction)
             system))
       (lift-thei input instruction system)))))

(defmethod derive-types ((instruction bir:thei) system)
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
    (let ((derived-a-type (derive-type-for-linear-datum
     (bir:output instruction)
     (if (eq type-check-function nil)
         ctype
         (ctype:values-conjoin system (bir:asserted-type instruction) ctype))
     system)))
      ;; If we didn't derive a type for this datum, and we've analyzed the
      ;; instruction before, then we can ignore it for now.
      (when (and (not derived-a-type) (not *is-first-pass*))
        (setf (:should-process instruction) NIL)))
  
    ;; The asserted type can be propagated even if it's not checked.
    (assert-type-for-linear-datum
     (bir:output instruction)
     (ctype:values-conjoin system (bir:asserted-type instruction)
                           (bir:asserted-type input))
     system)
    ;; Propagate the type of the input into function.
    (unless (symbolp type-check-function)
      (derive-local-call-parameter-types type-check-function
                                         (list ctype) system))))

;; Clients can specialize this to perform specific transformations on
;; the IR for a call.
;; The "transform" object is some thing stored in a function's attributes,
;; and so is client-defined. The attributes system has more info.
;; Methods should return true if a transformation took place, and otherwise
;; return false.
(defgeneric transform-call (system transform call)
  (:method (system transform (call bir:abstract-call))
    (declare (ignore system transform))
    nil))

(defgeneric fold-call (system fold call arguments)
  (:method (system fold (call bir:abstract-call) arguments)
    (declare (ignore system fold arguments))
    nil))

;;; Given a non-values ctype, returns two values:
;;; The value of the constant type, or NIL if it's not constant
;;; A boolean that's true iff it is constant
;;; FIXME: Move to ctype?
(defun constant-type-value (ct system)
  (cond ((ctype:member-p system ct)
         (let ((membs (ctype:member-members system ct)))
           (if (= (length membs) 1)
               (values (elt membs 0) t)
               (values nil nil))))
        ((ctype:rangep ct system)
         (multiple-value-bind (low lxp) (ctype:range-low ct system)
           (multiple-value-bind (high hxp) (ctype:range-high ct system)
             (if (or lxp hxp (not low) (not high) (not (eql low high)))
                 (values nil nil)
                 (values low t)))))
        (t (values nil nil))))

(defun constant-arguments (arguments system)
  (loop for arg in arguments
        for ct = (ctype:primary (bir:ctype arg) system)
        collect (multiple-value-bind (cvalue constantp)
                    (constant-type-value ct system)
                  (if constantp
                      cvalue
                      (return (values nil nil))))
          into rargs
        finally (return (values rargs t))))

(defun maybe-fold-call-1 (fold arguments instruction system)
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
                            for ct = (ctype:member system rv)
                            for vct = (ctype:single-value ct system)
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
    (fold-call system fold instruction arguments)))

(defun maybe-fold-call (folds arguments instruction system)
  (when (not (null folds))
    (multiple-value-bind (args validp) (constant-arguments arguments system)
      (when validp
        (loop for fold in folds
                thereis (maybe-fold-call-1 fold args instruction system))))))

(defmethod meta-evaluate-instruction ((instruction bir:call) system)
  (let* ((attr (bir:attributes instruction))
         (identities (attributes:identities attr)))
    (or
     ;; Try all client constant folds in order.
     ;; Folding is always preferable to transformation, so we try it first.
     (maybe-fold-call identities
                      (rest (bir:inputs instruction))
                      instruction system)
     ;; Try all client transforms in order.
     ;; If any return true, a change has been made.
     (some (lambda (identity) (transform-call system identity instruction))
           identities)
     ;; If we've determined this call never returns, axe later code.
     (maybe-unreach-call instruction system))))

(defmethod meta-evaluate-instruction ((instruction bir:primop) system)
  (let* ((attr (bir:attributes instruction))
         (identities (attributes:identities attr)))
    (or
     (maybe-fold-call identities (bir:inputs instruction)
                      instruction system)
     (some (lambda (identity) (transform-call system identity instruction))
           identities)
     (maybe-unreach-call instruction system))))

;;; If we've derived the output of a call to be bottom type, the call never
;;; returns. Delete subsequent code, unless it's already an UNREACHABLE
;;; instruction in which case we don't need to do anything.
(defun maybe-unreach-call (call system)
  (when (and ;; This works on primops, so try all outputs.
         (loop for out in (bir:outputs call)
               thereis (ctype:values-bottom-p (bir:ctype out) system))
         (not (typep (bir:successor call) 'bir:unreachable)))
    ;; INSERT-UNREACHABLE-AFTER also checks the next instruction, but as it
    ;; happens we need to make sure we return T if we made a change, so we
    ;; do a redundant check. FIXME.
    (insert-unreachable-after call)
    t))

(defun constant-mv-arguments (vct system)
  (if (and (null (ctype:values-optional vct system))
           (ctype:bottom-p (ctype:values-rest vct system) system))
      (loop for ct in (ctype:values-required vct system)
            collect (multiple-value-bind (cvalue constantp)
                        (constant-type-value ct system)
                      (if constantp
                          cvalue
                          (return (values nil nil))))
              into rargs
            finally (return (values rargs t)))
      (values nil nil)))

(defun maybe-fold-mv-call (folds input-type instruction system)
  (when (not (null folds))
    (multiple-value-bind (args validp)
        (constant-mv-arguments input-type system)
      (when validp
        (loop for fold in folds
                thereis (maybe-fold-call-1 fold args instruction system))))))

;;; Reduce an mv-call to a normal call if all its inputs are single-valued.
(defun mv-call->call (mv-call sys)
  (let* ((args (second (bir:inputs mv-call)))
         (argsdef (and (typep args 'bir:output)
                       (bir:definition args))))
    (when (typep argsdef 'bir:values-collect)
      (let ((vcin (bir:inputs argsdef)))
        (flet ((svp (datum) ; single-value-p
                 (and (typep datum 'bir:output) ; can't rewrite w/o this
                      (let ((ct (bir:ctype datum)))
                        (and (= (length (ctype:values-required ct sys)) 1)
                             (null (ctype:values-optional ct sys))
                             (ctype:bottom-p (ctype:values-rest ct sys) sys))))))
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

(defmethod meta-evaluate-instruction ((instruction bir:mv-call) system)
  (let ((identities (attributes:identities (bir:attributes instruction))))
    (or
     (maybe-fold-mv-call identities
                         (append-input-types
                          (mapcar #'bir:ctype (rest (bir:inputs instruction)))
                          system)
                         instruction system)
     (some (lambda (identity) (transform-call system identity instruction))
           identities)
     (maybe-unreach-call instruction system)
     (mv-call->call instruction system))))

;;; Given an instruction, its identity (e.g. function name), the VALUES type
;;; representing the incoming arguments, and the system, return the type of
;;; the result.
;;; FIXME: This might warrant a more complex type system in order to
;;; allow combining information.
;;; For example, (if test #'+ #'-) could still be seen to return two
;;; floats if given a float.
(defgeneric derive-return-type (instruction identity argstype system))
(defmethod derive-return-type ((inst bir:abstract-call) identity
                               argstype system)
  (declare (ignore identity argstype))
  (ctype:values-top system))

(defmethod derive-return-type ((inst bir:primop) identity
                               argstype system)
  (declare (ignore identity argstype))
  (ctype:values-top system))

(defmethod derive-types ((inst bir:call) system)
  (let ((identities (attributes:identities (bir:attributes inst))))
    (flet ((intype (reader)
             (ctype:values (loop for arg in (rest (bir:inputs inst))
                                 for ct = (funcall reader arg)
                                 collect (ctype:primary ct system))
                           nil (ctype:bottom system) system))
           (compute (identity intype)
             (derive-return-type inst identity intype system)))
      (cond ((null identities))
          ((= (length identities) 1)
           (let ((derived-a-type (derive-type-for-linear-datum
            (bir:output inst)
            (compute (first identities) (intype #'bir:ctype))
            system)))
             ;; If we didn't derive a type for this datum, and we've
             ;; analyzed the instruction before, then we can ignore it
             ;; for now.
             (when (and (not derived-a-type) (not *is-first-pass*))
               (setf (:should-process inst) NIL)))
          (assert-type-for-linear-datum
            (bir:output inst)
            (compute (first identities) (intype #'bir:asserted-type))
            system))
          (t
           (let ((dtypes
                   (loop for identity in identities
                         collect (compute identity (intype #'bir:ctype))))
                 (atypes
                   (loop for identity in identities
                         collect (compute identity (intype #'bir:asserted-type)))))
             (let ((derived-a-type (derive-type-for-linear-datum (bir:output inst)
                                           (apply #'ctype:values-conjoin
                                                  system dtypes)
                                           system)))
               ;; If we didn't derive a type for this datum, and we've
               ;; analyzed the instruction before, then we can ignore it
               ;; for now.
               (when (and (not derived-a-type) (not *is-first-pass*))
                 (setf (:should-process inst) NIL)))
             (assert-type-for-linear-datum (bir:output inst)
                                           (apply #'ctype:values-conjoin
                                                  system atypes)
                                           system)))))))

(defmethod derive-types ((inst bir:mv-call) system)
  (let ((identities (attributes:identities (bir:attributes inst))))
    (flet ((intype (reader)
             (append-input-types (mapcar reader (rest (bir:inputs inst))) system)))
    (cond ((null identities))
          ((= (length identities) 1)
           (let ((derived-a-type (derive-type-for-linear-datum
            (bir:output inst)
            (derive-return-type inst (first identities) (intype #'bir:ctype) system)
            system)))
             ;; If we didn't derive a type for this datum, and we've analyzed
             ;; the instruction before, then we can ignore it for now.
             (when (and (not derived-a-type) (not *is-first-pass*))
               (setf (:should-process inst) NIL)))
           (assert-type-for-linear-datum
            (bir:output inst)
            (derive-return-type inst (first identities) (intype #'bir:asserted-type)
                                system)
            system))
          (t
           (let ((dtypes
                   (loop with arg = (intype #'bir:ctype)
                         for identity in identities
                         collect (derive-return-type inst identity arg system)))
                 (atypes
                   (loop with arg = (intype #'bir:asserted-type)
                         for identity in identities
                         collect (derive-return-type inst identity arg system))))
             (let ((derived-a-type (or (derive-type-for-linear-datum (bir:output inst)
                                           (apply #'ctype:values-conjoin
                                                  system dtypes)
                                           system)
             (derive-type-for-linear-datum (bir:output inst)
                                           (apply #'ctype:values-conjoin
                                                  system atypes)
                                           system))))
               ;; If we didn't derive a type for this datum, and we've
               ;; analyzed the instruction before, then we can ignore it
               ;; for now.
               (when (and (not derived-a-type) (not *is-first-pass*))
                 (setf (:should-process inst) NIL)))))))))

(defmethod derive-types ((inst bir:primop) system)
  ;; Only do this when there's exactly one output.
  ;; derive-return-type doesn't make much sense otherwise.
  (when (= (length (bir:outputs inst)) 1)
    (let ((identities (attributes:identities (bir:attributes inst))))
      (flet ((intype (reader)
               (ctype:values (loop for arg in (rest (bir:inputs inst))
                                   for ct = (funcall reader arg)
                                   collect (ctype:primary ct system))
                             nil (ctype:bottom system) system))
             (compute (identity intype)
               (derive-return-type inst identity intype system)))
        (cond ((null identities))
              ((= (length identities) 1)
               (let ((derived-a-type (derive-type-for-linear-datum
                (bir:output inst)
                (compute (first identities) (intype #'bir:ctype))
                system)))
                 ;; If we didn't derive a type for this datum, and we've
                 ;; analyzed the instruction before, then we can ignore it
                 ;; for now.
                 (when (and (not derived-a-type) (not *is-first-pass*))
                   (setf (:should-process inst) NIL)))
               (assert-type-for-linear-datum
                (bir:output inst)
                (compute (first identities) (intype #'bir:asserted-type))
                system))
              (t
               (let ((dtypes
                       (loop for identity in identities
                             collect (compute identity (intype #'bir:ctype))))
                     (atypes
                       (loop for identity in identities
                             collect (compute identity (intype #'bir:asserted-type)))))
                 (let ((derived-a-type (derive-type-for-linear-datum (bir:output inst)
                                               (apply #'ctype:values-conjoin
                                                      system dtypes)
                                               system)))
                   ;; If we didn't derive a type for this datum, and we've
                   ;; analyzed the instruction before, then we can ignore it
                   ;; for now.
                   (when (and (not derived-a-type) (not *is-first-pass*))
                     (setf (:should-process inst) NIL)))
                 (assert-type-for-linear-datum (bir:output inst)
                                               (apply #'ctype:values-conjoin
                                                      system atypes)
                                               system))))))))

(defmethod meta-evaluate-instruction ((inst bir:abstract-local-call) system)
  (declare (ignore system))
  ;; If a local function doesn't return, mark subsequent code unreachable
  ;; (unless we have already done so)
  (when (null (bir:returni (bir:callee inst)))
    (insert-unreachable-after inst)
    t))
