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
    (cleavir-bir:do-functions (function module)
      (meta-evaluate-function function system)
      (cleavir-bir:compute-iblock-flow-order function))))

;;; Prove that LINEAR-DATUM is of type DERIVED-TYPE.
(defun derive-type-for-linear-datum (linear-datum derived-type system)
  (setf (cleavir-bir:derived-type linear-datum)
        (cleavir-ctype:values-conjoin (cleavir-bir:ctype linear-datum)
                                      derived-type system)))

;;; Derive the type of the function arguments from the types of the
;;; arguments of its local calls.
(defun derive-function-argument-types (function system)
  (if (cleavir-bir:enclose function)
      ;; If there is an enclose, we can be called from pretty much anywhere,
      ;; so there's not much we can determine about the arguments. We can mark
      ;; &rest arguments as being lists, at least.
      (cleavir-bir:map-lambda-list
       (lambda (state item index)
         (declare (ignore index))
         (when (eq state '&rest)
           (derive-type-for-linear-datum
            item
            (let ((top-ctype (cleavir-ctype:top system)))
              ;; LIST is of course (or null cons)
              (cleavir-ctype:single-value
               (cleavir-ctype:disjoin/2
                (cleavir-ctype:member system nil)
                (cleavir-ctype:cons top-ctype top-ctype system)
                system)
               system))
            system)))
       (cleavir-bir:lambda-list function))
      ;; If there are no local calls either, don't bother doing
      ;; anything, especially since we're deriving the type from scratch
      ;; optimistically.
      (let ((local-calls (cleavir-bir:local-calls function)))
        (unless (cleavir-set:notany
                 ;; Dunno how to mess with mv-local-call yet.
                 (lambda (call) (typep call 'cleavir-bir:local-call))
                 local-calls)
          (cleavir-bir:map-lambda-list
           (lambda (state item index)
             (case state
               ((:required &optional)
                (let ((type (cleavir-ctype:bottom system))
                      (suppliedp (cleavir-ctype:bottom system)))
                  (cleavir-set:doset (local-call local-calls)
                    (let ((arg
                            (nth index (rest (cleavir-bir:inputs local-call)))))
                      (setq type
                            (cleavir-ctype:disjoin/2
                             type
                             (if arg
                                 (cleavir-ctype:primary
                                  (cleavir-bir:ctype arg) system)
                                 (cleavir-ctype:member system nil))
                             system))
                      (setq suppliedp
                            (cleavir-ctype:disjoin/2
                             suppliedp
                             (if arg
                                 (cleavir-ctype:member system t)
                                 (cleavir-ctype:member system nil))
                             system))))
                  (ecase state
                    (:required
                     (setf (cleavir-bir:derived-type item)
                           (cleavir-ctype:single-value type system)))
                    (&optional
                     (setf (cleavir-bir:derived-type (first item))
                           (cleavir-ctype:single-value type system))
                     (setf (cleavir-bir:derived-type (second item))
                           (cleavir-ctype:single-value suppliedp system))))))
               (&key
                ;; too hairy for me to handle
                )))
           (cleavir-bir:lambda-list function))))))

(defun meta-evaluate-function (function system)
  (derive-function-argument-types function system)
  ;; The decision for what goes in the forward vs backward flow passes
  ;; has to do with whether the effects of the optimization are on
  ;; things that happen before vs after in the flow graph, and if that
  ;; doesn't matter, which order makes it more likely to fire, so we
  ;; can feed effects as much as possible in one pass.
  (cleavir-bir:do-iblocks (iblock function)
    ;; Make sure to merge the successors as much as possible so we can
    ;; trigger more optimizations.
    (loop while (cleavir-bir:merge-successor-if-possible iblock))
    (meta-evaluate-iblock iblock system))
  (cleavir-bir:do-iblocks (iblock function :backward)
    (flush-dead-code iblock)
    (let ((end (cleavir-bir:end iblock)))
      (typecase end
        (cleavir-bir:ifi
         (or (eliminate-if-if end)
             (eliminate-degenerate-if end)))
        (cleavir-bir:jump
         (cleavir-bir:delete-iblock-if-empty iblock))))))

;;; Derive the types of any iblock inputs. We have to do this from
;;; scratch optimistically because we are disjoining the types of the
;;; definitions, instead of narrowing the types conservatively.
(defun derive-iblock-input-types (iblock system)
  (dolist (phi (cleavir-bir:inputs iblock))
    (let ((type (cleavir-ctype:values
                 nil nil (cleavir-ctype:bottom system) system)))
      (cleavir-set:doset (definition (cleavir-bir:definitions phi))
        (setq type
              (cleavir-ctype:values-disjoin
               type
               (cleavir-bir:ctype
                (nth (position phi (cleavir-bir:outputs definition))
                     (cleavir-bir:inputs definition)))
               system)))
      (setf (cleavir-bir:derived-type phi) type))))

(defun meta-evaluate-iblock (iblock system)
  (derive-iblock-input-types iblock system)
  (cleavir-bir:do-iblock-instructions (instruction iblock)
    (unless (meta-evaluate-instruction instruction system)
      (derive-types instruction system))))

(defgeneric maybe-flush-instruction (instruction))

(defmethod maybe-flush-instruction ((instruction cleavir-bir:instruction)))

(defmethod maybe-flush-instruction ((instruction cleavir-bir:readvar))
  (when (cleavir-bir:unused-p (cleavir-bir:output instruction))
    (cleavir-bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction
    ((instruction cleavir-bir:constant-reference))
  (when (cleavir-bir:unused-p (cleavir-bir:output instruction))
    (cleavir-bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction ((instruction cleavir-bir:enclose))
  (when (cleavir-bir:unused-p (cleavir-bir:output instruction))
    (cleavir-bir:delete-instruction instruction)))
(defmethod maybe-flush-instruction ((instruction cleavir-bir:conditional-test))
  (when (cleavir-bir:unused-p (cleavir-bir:output instruction))
    (cleavir-bir:delete-instruction instruction)))

(defmethod maybe-flush-instruction ((instruction cleavir-bir:abstract-call))
  (when (and (cleavir-bir:unused-p (cleavir-bir:output instruction))
             (cleavir-attributes:has-boolean-attribute-p
              (cleavir-bir:attributes instruction)
              :flushable))
    (cleavir-bir:delete-instruction instruction)))

(defmethod maybe-flush-instruction ((instruction cleavir-bir:vprimop))
  (let ((name (cleavir-primop-info:name (cleavir-bir:info instruction))))
    (when (and (member name '(fdefinition car cdr symbol-value))
               (cleavir-bir:unused-p (first (cleavir-bir:outputs instruction))))
      (cleavir-bir:delete-instruction instruction))))

(defun flush-dead-code (iblock)
  (cleavir-bir:map-iblock-instructions-backwards #'maybe-flush-instruction
                                                 iblock)
  (dolist (phi (cleavir-bir:inputs iblock))
    (when (null (cleavir-bir:use phi))
      (cleavir-bir:delete-phi phi))))

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
  (let* ((in (cleavir-bir:input instruction))
         (inct (cleavir-ctype:primary (cleavir-bir:ctype in) system))
         (next (cleavir-bir:next instruction))
         (then (first next))
         (else (second next)))
    (multiple-value-bind (next dead)
        (cond ((cleavir-ctype:disjointp inct
                                        (cleavir-ctype:member system nil)
                                        system)
               #+(or)
               (format t "folding ifi based on type ~a" (cleavir-bir:ctype in))
               (values then else))
              ((cleavir-ctype:subtypep inct
                                       (cleavir-ctype:member system nil)
                                       system)
               #+(or)
               (print "folding ifi based on type NULL")
               (values else then)))
      (when dead
        #+(or)
        (print "folding ifi instruction")
        (cleavir-bir:replace-terminator
         (make-instance 'cleavir-bir:jump
                        :next (list next)
                        :inputs '() :outputs '())
         instruction)
        ;; Try to delete the block if possible, so we can maybe
        ;; optimize more in this pass. Ultimately, the flow order will
        ;; be recomputed.
        (cleavir-bir:maybe-delete-iblock dead)
        t))))

;;; Eliminate IF IF constructs. Does the equivalent of (IF
;;; (IF X Y Z) A B) => (IF X (IF Y A B) (IF Z A B)). The reason this
;;; is optimization is desirable is that control flow is simplified,
;;; and also the flow of values is simplified by eliminating a phi
;;; which can lead to further optimization.
(defun eliminate-if-if (instruction)
  (let* ((iblock (cleavir-bir:iblock instruction))
         (phis (cleavir-bir:inputs iblock))
         (test (cleavir-bir:input instruction)))
    ;; An IFI is eligible for this optimization if it starts its block
    ;; (i.e. is the only instruction in the block) and tests the phi
    ;; which is the unique input to its block.
    (when (and (eq instruction (cleavir-bir:start iblock))
               (null (rest phis))
               (eq test (first phis)))
      #+(or)
      (print "eliminating if-if!")
      ;; We duplicate the IFI and replace the terminators for every
      ;; predecessor.
      (let ((next (cleavir-bir:next instruction))
            (origin (cleavir-bir:origin instruction))
            (predecessors (cleavir-bir:predecessors iblock)))
        ;; If one of the predecessors is an unwind, don't replace it
        (cleavir-set:doset (predecessor predecessors)
          (let ((end (cleavir-bir:end predecessor)))
            (when (cleavir-bir:unwindp end)
              (return-from eliminate-if-if nil))))
        ;; Actual work
        (cleavir-set:doset (predecessor predecessors)
          (let* ((end (cleavir-bir:end predecessor))
                 (input (first (cleavir-bir:inputs end))))
            (assert (not (cleavir-bir:unwindp end))
                    ()
                    "Don't replace jumps with unwinding action!")
            (assert (and (null (rest (cleavir-bir:outputs end)))
                         (eq (first (cleavir-bir:outputs end)) test))
                    ()
                    "Jump/phi pair inconsistent.")
            (let ((ifi (make-instance 'cleavir-bir:ifi
                         :next (copy-list next)
                         :origin origin)))
              (cleavir-bir:replace-terminator ifi end)
              (setf (cleavir-bir:inputs ifi) (list input))))))
      ;; Now we clean up the original IFI block.
      (cleavir-bir:delete-iblock iblock)
      t)))

;;; Eliminate an IF if the two successors of IF are the same.
(defun eliminate-degenerate-if (ifi)
  (let* ((next (cleavir-bir:next ifi))
         (succ (first next)))
    (when (eq succ (second next))
      #+(or)
      (print "ifi same block -> jump optimization")
      (change-class ifi 'cleavir-bir:jump :outputs () :inputs ()
        :next (list succ)))))

(defmethod meta-evaluate-instruction ((instruction cleavir-bir:ifi) system)
  (fold-ifi instruction system))

;; Replace COMPUTATION with a constant reference to value.
(defun replace-computation-by-constant-value (instruction value)
  (let* ((constant 
           (cleavir-bir:constant-in-module
            value
            (cleavir-bir:module (cleavir-bir:function instruction))))
         (constant-reference
           (make-instance 'cleavir-bir:constant-reference
             :inputs (list constant)))
         (outs (cleavir-bir:outputs instruction)))
    (setf (cleavir-bir:outputs instruction) nil
          (cleavir-bir:outputs constant-reference) outs)
    (cleavir-bir:insert-instruction-before constant-reference instruction)
    (cleavir-bir:delete-instruction instruction)))

;; Try to constant fold an instruction on INPUTS by applying FOLDER on its
;; inputs.
(defun constant-fold-instruction (instruction inputs folder)
  (let ((definers (loop for inp in inputs
                        if (typep inp 'cleavir-bir:output)
                          collect (cleavir-bir:definition inp)
                        else do (return-from constant-fold-instruction nil))))
    (when (every (lambda (definer)
                   (typep definer 'cleavir-bir:constant-reference))
                 definers)
      (replace-computation-by-constant-value
       instruction
       (apply folder
              (mapcar (lambda (def)
                        (cleavir-bir:constant-value (cleavir-bir:input def)))
                      definers)))
      t)))

;;; If there is only one input and it has type (values something &rest nil),
;;; there is no need for this instruction.
(defmethod meta-evaluate-instruction ((inst cleavir-bir:fixed-to-multiple)
                                      system)
  (let ((inputs (cleavir-bir:inputs inst)))
    (when (= (length inputs) 1)
      (let* ((input (first inputs))
             (inty (cleavir-bir:ctype input)))
        (when (and (cleavir-ctype:bottom-p
                    (cleavir-ctype:values-rest inty system)
                    system)
                   (null (cleavir-ctype:values-optional inty system))
                   (= (length (cleavir-ctype:values-required inty system)) 1))
          (setf (cleavir-bir:inputs inst) nil)
          (let ((out (cleavir-bir:output inst)))
            (cleavir-bir:replace-uses input out)
            (cleavir-bir:delete-instruction inst)
            t))))))

(defmethod derive-types ((instruction cleavir-bir:fixed-to-multiple) system)
  (derive-type-for-linear-datum
   (cleavir-bir:output instruction)
   (cleavir-ctype:values
    (loop for inp in (cleavir-bir:inputs instruction)
          collect (cleavir-ctype:primary (cleavir-bir:ctype inp) system))
    nil
    (cleavir-ctype:bottom system)
    system)
   system))

(defmethod meta-evaluate-instruction ((instruction cleavir-bir:eq-test) system)
  (declare (ignore system))
  (let ((inputs (cleavir-bir:inputs instruction)))
    (constant-fold-instruction instruction inputs #'eq)))

(defmethod meta-evaluate-instruction
    ((instruction cleavir-bir:typeq-test) system)
  (let ((ctype (cleavir-ctype:primary
                (cleavir-bir:ctype (cleavir-bir:input instruction)) system))
        (test-ctype (cleavir-bir:test-ctype instruction)))
    (cond ((cleavir-ctype:subtypep ctype test-ctype system)
           (replace-computation-by-constant-value instruction t)
           t)
          ((cleavir-ctype:disjointp ctype test-ctype system)
           (replace-computation-by-constant-value instruction nil)
           t))))

(defmethod derive-types ((instruction cleavir-bir:constant-reference) system)
  (derive-type-for-linear-datum
   (cleavir-bir:output instruction)
   (cleavir-ctype:single-value
    (cleavir-ctype:member system (cleavir-bir:constant-value
                                  (cleavir-bir:input instruction)))
    system)
   system))

;;; Local variable with one reader and one writer can be substituted
;;; away,
(defun substitute-single-read-variable-if-possible (variable system)
  (let ((readers (cleavir-bir:readers variable)))
    (when (and (cleavir-bir:immutablep variable)
               (= (cleavir-set:size readers) 1))
      (let* ((binder (cleavir-bir:binder variable))
             (reader (cleavir-set:arb readers))
             (reader-out (cleavir-bir:output reader)))
        (when (eq (cleavir-bir:function binder)
                  (cleavir-bir:function reader))
          #+(or)
          (format t "~&meta-evaluate: substituting single read binding of ~a" variable)
          (let* ((input (cleavir-bir:input binder))
                 (type (cleavir-ctype:coerce-to-values
                        (cleavir-ctype:primary
                         (cleavir-bir:ctype input)
                         system)
                        system))
                 (fout (make-instance 'cleavir-bir:output
                         :derived-type type))
                 (ftm (make-instance 'cleavir-bir:fixed-to-multiple
                        :outputs (list fout))))
            (setf (cleavir-bir:inputs binder) nil)
            (cleavir-bir:insert-instruction-before ftm reader)
            (cleavir-bir:replace-uses fout reader-out)
            (setf (cleavir-bir:inputs ftm) (list input)))
          (cleavir-bir:delete-instruction reader)
          t)))))

;;; Variable bound to constant can get propagated.
(defun constant-propagate-variable-if-possible (variable)
  (when (cleavir-bir:immutablep variable)
    (let* ((writer (cleavir-bir:binder variable))
           (input (cleavir-bir:input writer)))
      (when (typep input 'cleavir-bir:output) ; FIXME: should be SSA?
        (let ((def (cleavir-bir:definition input)))
          ;; FIXME: Should really check for a constant type here instead.
          (typecase def
            (cleavir-bir:constant-reference
             (let ((constant (cleavir-bir:input def)))
               (cleavir-set:doset (reader (cleavir-bir:readers variable))
                 (change-class reader 'cleavir-bir:constant-reference
                               :inputs (list constant)))
               #+(or)
               (format t "~&meta-evaluate: constant propagating ~a"
                       (cleavir-bir:constant-value constant))
               (cleavir-bir:delete-instruction writer)
               (cleavir-bir:delete-instruction def))
             t)
            (t nil)))))))

;;; For an immutable variable, we prove that the type of its readers
;;; is just the type of its definition.
(defun derive-type-for-variable (variable system)
  (let* ((dtype (if (cleavir-bir:immutablep variable)
                    ;; We can get the type from the definition.
                    (cleavir-ctype:primary
                     (cleavir-bir:ctype
                      (cleavir-bir:input (cleavir-bir:binder variable)))
                     system)
                    ;; Can't use the definition's type, but we can still
                    ;; derive that it's exactly one value.
                    (cleavir-ctype:top system)))
         (type (cleavir-ctype:single-value dtype system)))
      (cleavir-set:doset (reader (cleavir-bir:readers variable))
        (let ((out (cleavir-bir:output reader)))
          (derive-type-for-linear-datum out type system)))))

(defmethod meta-evaluate-instruction ((instruction cleavir-bir:leti) system)
  (let ((variable (cleavir-bir:output instruction)))
    (when variable
      (or (substitute-single-read-variable-if-possible variable system)
          (constant-propagate-variable-if-possible variable)))))

(defmethod derive-types ((instruction cleavir-bir:leti) system)
  (let ((variable (cleavir-bir:output instruction)))
    (when variable (derive-type-for-variable variable system))))

(defmethod derive-types ((instruction cleavir-bir:returni) system)
  ;; Propagate the return type to local calls and enclose of the function.
  (let ((function (cleavir-bir:function instruction))
        (return-type
          (cleavir-bir:ctype (cleavir-bir:input instruction))))
    (cleavir-set:doset (local-call (cleavir-bir:local-calls function))
      (derive-type-for-linear-datum
       (cleavir-bir:output local-call)
       return-type
       system))))

(defmethod derive-types ((instruction cleavir-bir:enclose) system)
  (let ((ftype (cleavir-ctype:single-value
                (cleavir-ctype:function-top system) system)))
    (derive-type-for-linear-datum (cleavir-bir:output instruction)
                                  ftype system)))

(defmethod derive-types ((instruction cleavir-bir:values-save) system)
  (derive-type-for-linear-datum (cleavir-bir:output instruction)
                                (cleavir-bir:ctype
                                 (cleavir-bir:input instruction))
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
    (let ((req1 (cleavir-ctype:values-required ct1 system))
          (opt1 (cleavir-ctype:values-optional ct1 system))
          (rest1 (cleavir-ctype:values-rest ct1 system))
          (req2 (cleavir-ctype:values-required ct2 system))
          (opt2 (cleavir-ctype:values-optional ct2 system))
          (rest2 (cleavir-ctype:values-rest ct2 system)))
      (if (and (null opt1) (cleavir-ctype:bottom-p rest1 system))
          ;; simple case
          (cleavir-ctype:values (append req1 req2) opt2 rest2 system)
          ;; Approximate as described
          (cleavir-ctype:values
           (append req1 (make-list (length req2)
                                   :initial-element (cleavir-ctype:top system)))
           nil
           (cleavir-ctype:top system)
           system)))))

(defmethod derive-types ((instruction cleavir-bir:values-collect) system)
  (let* ((ins (mapcar #'cleavir-bir:ctype (cleavir-bir:inputs instruction)))
         (out (cleavir-bir:output instruction))
         (ct
           (cond ((zerop (length ins)) ; degenerate cases
                  (cleavir-ctype:values
                   nil nil (cleavir-ctype:bottom system) system))
                 ((= (length ins) 1) (first ins))
                 (t (reduce (values-ctype-appender system) ins)))))
    (derive-type-for-linear-datum out ct system)))

(defmethod meta-evaluate-instruction ((instruction cleavir-bir:thei) system)
  (let ((ctype (cleavir-bir:ctype (cleavir-bir:input instruction))))
    ;; Remove THEI when its input's type is a subtype of the
    ;; THEI's asserted type.
    (when (cleavir-ctype:values-subtypep
           ctype (cleavir-bir:asserted-type instruction) system)
      (cleavir-bir:delete-thei instruction)
      t)))

(defmethod derive-types ((instruction cleavir-bir:thei) system)
  (let* ((type-check-function
           (cleavir-bir:type-check-function instruction))
         (input (cleavir-bir:input instruction))
         (ctype (cleavir-bir:ctype input)))
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
     (cleavir-bir:output instruction)
     (if (eq type-check-function :external)
         ctype
         (cleavir-ctype:values-conjoin
          (cleavir-bir:asserted-type instruction) ctype system))
     system)
    ;; Propagate the type of the input into function.
    ;; FIXME: Extend this to values types.
    (unless (symbolp type-check-function)
      (derive-type-for-linear-datum
       (first (cleavir-bir:lambda-list type-check-function))
       ctype system))))
