;;;; The idea of meta-evaluation on BIR is that they are mostly
;;;; bottom-up and require no flow analysis, corresponding to
;;;; expression level optimizations and propagation on the original
;;;; expression tree. Loosely based on the design of ir1opt.lisp by
;;;; Rob MacLachlan in CMU CL.

(in-package #:cleavir-bir-transformations)

(defun meta-evaluate-module (module system)
  ;; Obviously this should actually be a worklist algorithm and not
  ;; just two or three passes. We repeat on the module level so that
  ;; types are more likely to get propagated interprocedurally.
  (dotimes (repeat 3)
    (declare (ignorable repeat))
    (bir:do-functions (function module)
      ;; This check is necessary because meta-evaluation might have deleted
      ;; the function during our iteration. KLUDGE?
      (when (set:presentp function (bir:functions module))
        (meta-evaluate-function function system)
        (bir:compute-iblock-flow-order function)))))

;;; Prove that LINEAR-DATUM is of type DERIVED-TYPE.
(defun derive-type-for-linear-datum (linear-datum derived-type system)
  (setf (bir:derived-type linear-datum)
        (ctype:values-conjoin (bir:ctype linear-datum) derived-type system)))

(defun derive-attributes (linear-datum new-attributes)
  (setf (bir:attributes linear-datum)
        ;; join due to contravariance
        (attributes:join-attributes
         (bir:attributes linear-datum) new-attributes)))

;;; Derive the type of the function arguments from the types of the
;;; arguments of its local calls.
(defun derive-function-argument-types (function system)
  (if (bir:enclose function)
      ;; If there is an enclose, we can be called from pretty much anywhere,
      ;; so there's not much we can determine about the arguments. We can mark
      ;; &rest arguments as being lists, and everything as being single values,
      ;; at least.
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
      ;; If there are no local calls either, don't bother doing
      ;; anything, especially since we're deriving the type from scratch
      ;; optimistically.
      (let ((local-calls (bir:local-calls function)))
        (unless (set:notany
                 ;; Dunno how to mess with mv-local-call yet.
                 (lambda (call) (typep call 'bir:local-call))
                 local-calls)
          (bir:map-lambda-list
           (lambda (state item index)
             (case state
               ((:required &optional)
                (let ((type (ctype:bottom system))
                      (suppliedp (ctype:bottom system)))
                  (set:doset (local-call local-calls)
                    (let ((arg (nth index (rest (bir:inputs local-call)))))
                      (setq type
                            (ctype:disjoin/2
                             type
                             (if arg
                                 (ctype:primary (bir:ctype arg) system)
                                 (ctype:member system nil))
                             system))
                      (setq suppliedp
                            (ctype:disjoin/2
                             suppliedp
                             (if arg
                                 (ctype:member system t)
                                 (ctype:member system nil))
                             system))))
                  (ecase state
                    (:required
                     (setf (bir:derived-type item)
                           (ctype:single-value type system)))
                    (&optional
                     (setf (bir:derived-type (first item))
                           (ctype:single-value type system))
                     (setf (bir:derived-type (second item))
                           (ctype:single-value suppliedp system))))))
               (&key
                ;; too hairy for me to handle
                )))
           (bir:lambda-list function))))))

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
    (flush-dead-code iblock)
    (let ((end (bir:end iblock)))
      (typecase end
        (bir:ifi (or (eliminate-if-if end) (eliminate-degenerate-if end)))
        (bir:jump (bir:delete-iblock-if-empty iblock))))))

;;; Derive the types of any iblock inputs. We have to do this from
;;; scratch optimistically because we are disjoining the types of the
;;; definitions, instead of narrowing the types conservatively.
(defun derive-iblock-input-types (iblock system)
  (dolist (phi (bir:inputs iblock))
    (let ((definitions (bir:definitions phi))
          (type (ctype:values-bottom system)))
      (set:doset (definition definitions)
        (let ((input (nth (position phi (bir:outputs definition))
                          (bir:inputs definition))))
          (setq type
                (ctype:values-disjoin type (bir:ctype input) system))))
      (setf (bir:derived-type phi) type))))

(defun compute-phi-attributes (phi)
  ;; Unlike with types, we don't have a starting "all attributes" value
  ;; to use, so this is a little funky.
  (let ((definitions (bir:definitions phi)))
    (if (set:empty-set-p definitions)
        nil ; meaningless if there are no defs
        (let* ((sdef (set:arb definitions))
               (sinput (nth (position phi (bir:outputs sdef))
                            (bir:inputs sdef)))
               (attr (bir:attributes sinput)))
          (set:doset (definition definitions attr)
            (unless (eq definition sdef)
              (let ((input (nth (position phi (bir:outputs definition))
                                (bir:inputs definition))))
                ;; meet due to contravariance
                (setf attr (attributes:meet-attributes
                            attr
                            (bir:attributes input))))))))))

(defun derive-iblock-input-attributes (iblock)
  (dolist (phi (bir:inputs iblock))
    (setf (bir:attributes phi) (compute-phi-attributes phi))))

(defun meta-evaluate-iblock (iblock system)
  (derive-iblock-input-types iblock system)
  (derive-iblock-input-attributes iblock)
  (bir:do-iblock-instructions (instruction iblock)
    (unless (meta-evaluate-instruction instruction system)
      (derive-types instruction system))))

(defgeneric maybe-flush-instruction (instruction))

(defmethod maybe-flush-instruction ((instruction bir:instruction)))

(defmethod maybe-flush-instruction ((instruction bir:readvar))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction
    ((instruction bir:constant-reference))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction ((instruction bir:enclose))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction ((instruction bir:conditional-test))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction ((instruction bir:fixed-to-multiple))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction ((instruction bir:constant-reference))
  (when (bir:unused-p (bir:output instruction))
    (bir:delete-instruction instruction)))

(defmethod maybe-flush-instruction ((instruction bir:thei))
  (when (and (bir:unused-p (bir:output instruction))
             ;; NOTE: Haven't really thought this through. Maybe we
             ;; actually can delete THEIs even if they will be a type check?
             ;; Might depend on safety.
             (symbolp (bir:type-check-function instruction)))
    (bir:delete-instruction instruction)))

(defmethod maybe-flush-instruction ((instruction bir:abstract-call))
  (when (and (bir:unused-p (bir:output instruction))
             (attributes:has-flag-p (bir:attributes instruction) :flushable))
    (bir:delete-instruction instruction)))

(defmethod maybe-flush-instruction ((instruction bir:primop))
  (let ((outs (bir:outputs instruction)))
    (when (and (not (null outs))
               (bir:unused-p (first outs))
               (attributes:has-flag-p (bir:attributes instruction) :flushable))
      (bir:delete-instruction instruction))))

(defun flush-dead-code (iblock)
  (bir:map-iblock-instructions-backwards #'maybe-flush-instruction iblock)
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
    (derive-type-for-linear-datum
     (bir:output instruction)
     (ctype:values
      (loop for inp in inputs
            collect (ctype:primary (bir:ctype inp) system))
      nil
      (ctype:bottom system)
      system)
     system)))

(defmethod meta-evaluate-instruction ((instruction bir:eq-test) system)
  (declare (ignore system))
  (let ((inputs (bir:inputs instruction)))
    (constant-fold-instruction instruction inputs #'eq)))

(defmethod meta-evaluate-instruction
    ((instruction bir:typeq-test) system)
  (let ((ctype (ctype:primary
                (bir:ctype (bir:input instruction)) system))
        (test-ctype (bir:test-ctype instruction)))
    (cond ((ctype:subtypep ctype test-ctype system)
           (replace-computation-by-constant-value instruction t)
           t)
          ((ctype:disjointp ctype test-ctype system)
           (replace-computation-by-constant-value instruction nil)
           t))))

(defmethod derive-types ((instruction bir:constant-reference) system)
  (derive-type-for-linear-datum
   (bir:output instruction)
   (ctype:single-value
    (ctype:member system (bir:constant-value (bir:input instruction)))
    system)
   system))

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
                 (type (ctype:coerce-to-values
                        (ctype:primary (bir:ctype input) system)
                        system))
                 (fout (make-instance 'bir:output
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

;;; For an immutable variable, we prove that the type of its readers
;;; is just the type of its definition.
(defun derive-type-for-variable (variable system)
  (let* ((dtype (if (bir:immutablep variable)
                    ;; We can get the type from the definition.
                    (ctype:primary
                     (bir:ctype (bir:input (bir:binder variable)))
                     system)
                    ;; Can't use the definition's type, but we can still
                    ;; derive that it's exactly one value.
                    (ctype:top system)))
         (type (ctype:single-value dtype system)))
      (set:doset (reader (bir:readers variable))
        (let ((out (bir:output reader)))
          (derive-type-for-linear-datum out type system)))))

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
      (derive-type-for-variable variable system))))

(defmethod derive-types ((instruction bir:returni) system)
  ;; Propagate the return type to local calls and enclose of the function.
  (let ((function (bir:function instruction))
        (return-type (bir:ctype (bir:input instruction))))
    (set:doset (local-call (bir:local-calls function))
      (derive-type-for-linear-datum (bir:output local-call) return-type
                                    system))))

(defmethod derive-types ((instruction bir:enclose) system)
  (let ((ftype (ctype:single-value (ctype:function-top system) system)))
    (derive-type-for-linear-datum (bir:output instruction) ftype system)))

(defmethod derive-types ((instruction bir:values-save) system)
  (derive-type-for-linear-datum (bir:output instruction)
                                (bir:ctype (bir:input instruction))
                                system))

(defun values-ctype-appender (system)
  (lambda (ct1 ct2)
    ;; This function computes the type you get from appending two sets of
    ;; values together; in lisp terms, the type of
    ;; (multiple-value-call #'values a b) given the types of A and B.
    ;; This is considerably complicated by nontrivial &optional and &rest.
    ;; For a start (to be improved? FIXME) we take the required values of the
    ;; first form, and record the minimum number of required values, which is
    ;; just the sum of those of the values types.
    ;; Also, if the number of values of the first type is fixed (no &optional
    ;; and the &rest is bottom) we give the simple exact result.
    (let ((req1 (ctype:values-required ct1 system))
          (opt1 (ctype:values-optional ct1 system))
          (rest1 (ctype:values-rest ct1 system))
          (req2 (ctype:values-required ct2 system))
          (opt2 (ctype:values-optional ct2 system))
          (rest2 (ctype:values-rest ct2 system)))
      (if (and (null opt1) (ctype:bottom-p rest1 system))
          ;; simple case
          (ctype:values (append req1 req2) opt2 rest2 system)
          ;; Approximate as described
          (ctype:values
           (append req1 (make-list (length req2)
                                   :initial-element (ctype:top system)))
           nil
           (ctype:top system)
           system)))))

(defmethod derive-types ((instruction bir:values-collect) system)
  (let* ((ins (mapcar #'bir:ctype (bir:inputs instruction)))
         (out (bir:output instruction))
         (ct
           (cond ((zerop (length ins)) ; degenerate cases
                  (ctype:values nil nil (ctype:bottom system) system))
                 ((= (length ins) 1) (first ins))
                 (t (reduce (values-ctype-appender system) ins)))))
    (derive-type-for-linear-datum out ct system)))

(defmethod meta-evaluate-instruction ((instruction bir:thei) system)
  (let ((ctype (bir:ctype (bir:input instruction))))
    ;; Remove THEI when its input's type is a subtype of the
    ;; THEI's asserted type.
    (when (ctype:values-subtypep ctype (bir:asserted-type instruction) system)
      (bir:delete-thei instruction)
      t)))

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
    ;; checking. However, when the type check is marked as being done
    ;; externally, that means the compiler has not yet proven that the
    ;; asserted type holds, and so it must return the type of the
    ;; input. This gives us freedom to trust or explicitly check the
    ;; assertion as needed while making this decision transparent to
    ;; inference, and also type conflict when the type is checked
    ;; externally.
    (derive-type-for-linear-datum
     (bir:output instruction)
     (if (eq type-check-function :external)
         ctype
         (ctype:values-conjoin (bir:asserted-type instruction) ctype system))
     system)
    ;; Propagate the type of the input into function.
    ;; FIXME: Extend this to values types.
    (unless (symbolp type-check-function)
      (derive-type-for-linear-datum
       (first (bir:lambda-list type-check-function))
       ctype system))))

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

(defun constant-arguments (arguments system)
  (loop for arg in arguments
        for ct = (ctype:primary (bir:ctype arg) system)
        collect (if (ctype:member-p system ct)
                    (let ((membs (ctype:member-members system ct)))
                      (if (= (length membs) 1)
                          (elt membs 0)
                          (return-from constant-arguments (values nil nil))))
                    (return-from constant-arguments (values nil nil)))
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
  (let ((attr (bir:attributes instruction)))
    (or
     ;; Try all client constant folds in order.
     ;; Folding is always preferable to transformation, so we try it first.
     (maybe-fold-call (attributes:folds attr) (rest (bir:inputs instruction))
                      instruction system)
     ;; Try all client transforms in order.
     ;; If any return true, a change has been made.
     (some (lambda (transform) (transform-call system transform instruction))
           (attributes:transforms attr)))))

(defmethod meta-evaluate-instruction ((instruction bir:primop) system)
  (let ((attr (bir:attributes instruction)))
    (or
     (maybe-fold-call (attributes:folds attr) (bir:inputs instruction)
                      instruction system)
     (some (lambda (transform) (transform-call system transform instruction))
           (attributes:transforms (bir:attributes instruction))))))

(defgeneric derive-return-type (instruction deriver system))
(defmethod derive-return-type ((inst bir:abstract-call) deriver system)
  (declare (ignore deriver))
  (ctype:coerce-to-values (ctype:top system) system))

(defmethod derive-types ((inst bir:abstract-call) system)
  (let ((derivers (attributes:derivers (bir:attributes inst))))
    (cond ((null derivers))
          ((= (length derivers) 1)
           (derive-type-for-linear-datum (bir:output inst)
                                         (derive-return-type inst
                                                             (first derivers)
                                                             system)
                                         system))
          (t
           (let ((types (loop for deriver in derivers
                              collect (derive-return-type inst deriver system))))
             (derive-type-for-linear-datum (bir:output inst)
                                           (apply #'ctype:values-conjoin types)
                                           system))))))
