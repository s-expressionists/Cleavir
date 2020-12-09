;;;; The idea of meta-evaluation on BIR is that they are mostly
;;;; bottom-up and require no flow analysis, corresponding to
;;;; expression level optimizations and propagation on the original
;;;; expression tree. Loosely based on the design of ir1opt.lisp by
;;;; Rob MacLachlan in CMU CL.

(in-package #:cleavir-bir-transformations)

(defun meta-evaluate-module (module)
  ;; Obviously this should actually be a worklist algorithm and not
  ;; just two or three passes. We repeat on the module level so that
  ;; types are more likely to get propagated interprocedurally.
  (dotimes (repeat 3)
    (declare (ignore repeat))
    (cleavir-set:doset (function (cleavir-bir:functions module))
      (meta-evaluate-function function))))

;;; Derive the type of the function arguments from the types of the
;;; arguments of its local calls.
(defun derive-function-argument-types (function)
  (when (cleavir-set:empty-set-p (cleavir-bir:encloses function))
    ;; If there are no local calls either, don't bother doing
    ;; anything, especially since we're deriving the type from scratch
    ;; optimistically.
    (let ((local-calls (cleavir-bir:local-calls function)))
      (unless (cleavir-set:empty-set-p local-calls)
        (cleavir-bir:map-lambda-list
         (lambda (state item index)
           (case state
             ((:required &optional)
              (let ((type (cleavir-ctype:bottom nil))
                    (suppliedp (cleavir-ctype:bottom nil)))
                (cleavir-set:doset (local-call local-calls)
                  (unless (cleavir-bir:deletedp (cleavir-bir:iblock local-call))
                    (let ((arg (nth index (rest (cleavir-bir:inputs local-call)))))
                      (setq type
                            (cleavir-ctype:disjoin/2
                             type
                             (if arg
                                 (cleavir-bir:ctype arg)
                                 (cleavir-ctype:null-type nil))
                             nil))
                      (setq suppliedp
                            (cleavir-ctype:disjoin/2
                             suppliedp
                             (if arg
                                 (cleavir-ctype:member nil t)
                                 (cleavir-ctype:null-type nil))
                             nil)))))
                (ecase state
                  (:required
                   (setf (cleavir-bir:derived-type item) type))
                  (&optional
                   (setf (cleavir-bir:derived-type (first item)) type)
                   (setf (cleavir-bir:derived-type (second item)) suppliedp)))))
             (&key
              ;; too hairy for me to handle
              )))
         (cleavir-bir:lambda-list function))))))

(defun meta-evaluate-function (function)
  (derive-function-argument-types function)
  (dolist (iblock (cleavir-bir::iblocks-forward-flow-order function))
    ;; Make sure not to look at a block that might have been
    ;; deleted earlier in this forward pass.
    (unless (cleavir-bir:deletedp iblock)
      ;; Make sure to merge the successors as much as possible so we can
      ;; trigger more optimizations.
      (loop while (cleavir-bir:merge-successor-if-possible iblock))
      (meta-evaluate-iblock iblock)
      (flush-dead-code iblock)))
  (cleavir-bir:refresh-local-iblocks function))

;;; Derive the types of any iblock inputs. We have to do this from
;;; scratch optimistically because we are disjoining the types of the
;;; definitions, instead of narrowing the types conservatively.
(defun derive-iblock-input-types (iblock)
  (dolist (phi (cleavir-bir:inputs iblock))
    (let ((type (cleavir-ctype:bottom nil)))
      (dolist (definition (cleavir-bir:definitions phi))
        (unless (cleavir-bir:deletedp (cleavir-bir:iblock definition))
          (setq type
                (cleavir-ctype:disjoin/2
                 type
                 (cleavir-bir:ctype
                  (nth (position phi (cleavir-bir:outputs definition))
                       (cleavir-bir:inputs definition)))
                 nil))))
      (setf (cleavir-bir:derived-type phi) type))))

(defun meta-evaluate-iblock (iblock)
  (derive-iblock-input-types iblock)
  (cleavir-bir:do-iblock-instructions (instruction (cleavir-bir:start iblock))
    (meta-evaluate-instruction instruction)))

;; Remove dead code.
(defun flush-dead-code (iblock)
  (cleavir-bir:do-iblock-instructions (instruction (cleavir-bir:end iblock) :backward)
    (typecase instruction
      (cleavir-bir:multiple-to-fixed
       (when (every #'cleavir-bir:unused-p (cleavir-bir:outputs instruction))
         (cleavir-bir:delete-instruction instruction)))
      (cleavir-bir:computation
       (when (cleavir-bir:unused-p instruction)
         (typecase instruction
           ((or cleavir-bir:readvar cleavir-bir:constant-reference cleavir-bir:enclose)
            #+(or)
            (format t "~&meta-evaluate: flushing ~a" instruction)
            (cleavir-bir:delete-computation instruction))
           (cleavir-bir:abstract-call
            (when (cleavir-attributes:has-boolean-attribute-p
                   (cleavir-bir:attributes instruction)
                   :flushable)
              #+(or)
              (format t "~&meta-evaluate: flushing computation")
              (cleavir-bir:delete-computation instruction)))
           (cleavir-bir:vprimop
            (let ((name (cleavir-primop-info:name (cleavir-bir:info instruction))))
              (when (member name
                            '(fdefinition car cdr symbol-value))
                #+(or)
                (format t "~&meta-evaluate: flushing primop ~a" name)
                (cleavir-bir:delete-computation instruction))))))))))

(defgeneric meta-evaluate-instruction (instruction))

(defmethod meta-evaluate-instruction (instruction))

;;; Fold the IFI if we can determine whether or not the test will
;;; evaluate to NIL.
(defun fold-ifi (instruction)
  (let* ((iblock (cleavir-bir:iblock instruction))
         (test (first (cleavir-bir:inputs instruction)))
         (next (cleavir-bir:next instruction))
         (then (first next))
         (else (second next)))
    (multiple-value-bind (next dead)
        (cond ((typep test 'cleavir-bir:constant-reference)
               (if (cleavir-bir:constant-value (first (cleavir-bir:inputs test)))
                   (values then else)
                   (values else then)))
              ((multiple-value-bind (disjoint certain)
                   (cleavir-ctype:subtypep
                    (cleavir-ctype:conjoin/2 (cleavir-bir:ctype test)
                                             (cleavir-ctype:null-type nil)
                                             nil)
                    (cleavir-ctype:bottom nil)
                    nil)
                 (and disjoint certain))
               #+(or)
               (format t "folding ifi based on type ~a" (cleavir-bir:ctype test))
               (values then else))
              ((cleavir-ctype:subtypep (cleavir-bir:ctype test)
                                       (cleavir-ctype:null-type nil)
                                       nil)
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
        (cleavir-bir:merge-successor-if-possible iblock)
        ;; Try to delete the block if possible, so we can maybe
        ;; optimize more in this pass. Ultimately,
        ;; refresh-local-iblocks is supposed to flush dead blocks.
        (cleavir-bir:maybe-delete-iblock dead))
      t)))

;;; Eliminate degenerate if instructions. Does the equivalent of (IF
;;; (IF X Y Z) A B) => (IF X (IF Y A B) (IF Z A B)). The reason this
;;; is optimization is desirable is that control flow is simplified,
;;; and also the flow of values is simplified by eliminating a phi
;;; which can lead to further optimization.
(defun eliminate-degenerate-if (instruction)
  (let* ((iblock (cleavir-bir:iblock instruction))
         (phis (cleavir-bir:inputs iblock))
         (test (first (cleavir-bir:inputs instruction))))
    ;; A degenerate IFI starts its block (i.e. is the only instruction
    ;; in the block) and tests the phi which is the unique input to
    ;; its block.
    (when (and (eq instruction (cleavir-bir:start iblock))
               (null (rest phis))
               (eq test (first phis)))
      #+(or)
      (print "eliminating degenerate if!")
      ;; We duplicate the IFI and replace the terminators for every
      ;; predecessor.
      (let ((next (cleavir-bir:next instruction))
            (origin (cleavir-bir:origin instruction))
            (predecessors (cleavir-bir:predecessors iblock)))
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
      (cleavir-bir:delete-iblock iblock))))

(defmethod meta-evaluate-instruction ((instruction cleavir-bir:ifi))
  (unless (fold-ifi instruction)
    (eliminate-degenerate-if instruction)))

;; Replace COMPUTATION with a constant reference to value.
(defun replace-computation-by-constant-value (instruction value)
  (let ((constant-reference
          (cleavir-bir:make-constant-reference
           (cleavir-bir:constant-in-module
            value
            (cleavir-bir:module (cleavir-bir:function instruction))))))
    (cleavir-bir:insert-instruction-before constant-reference instruction)
    (cleavir-bir:replace-computation instruction constant-reference)))

;; Try to constant fold an instruction on INPUTS by applying FOLDER on its
;; inputs.
(defun constant-fold-instruction (instruction inputs folder)
  (when (every (lambda (input)
                 (typep input 'cleavir-bir:constant-reference))
               inputs)
    (replace-computation-by-constant-value
     instruction
     (apply folder
            (mapcar (lambda (input)
                      (cleavir-bir:constant-value
                       (first (cleavir-bir:inputs input))))
                    inputs)))
    t))

(defmethod meta-evaluate-instruction ((instruction cleavir-bir:multiple-to-fixed))
  (let ((definition (first (cleavir-bir:inputs instruction))))
    (cond ((typep definition 'cleavir-bir:fixed-to-multiple)
           (cleavir-bir:delete-transmission definition instruction)
           (cleavir-bir:delete-instruction definition))
          ;; Derive the type of the outputs (fixed values) from the
          ;; definition.
          (t
           (let ((values-type (cleavir-bir:ctype definition)))
             (unless (cleavir-ctype:top-p values-type nil)
               (let ((required-type (cleavir-ctype:values-required values-type nil))
                     (optional-type (cleavir-ctype:values-optional values-type nil))
                     (rest-type (cleavir-ctype:disjoin
                                 nil
                                 (cleavir-ctype:values-rest values-type nil)
                                 (cleavir-ctype:null-type nil))))
                 (dolist (output (cleavir-bir:outputs instruction))
                   (cleavir-bir:derive-type-for-linear-datum
                    output
                    (cond (required-type (pop required-type))
                          (optional-type (cleavir-ctype:disjoin/2
                                          (pop optional-type)
                                          (cleavir-ctype:null-type nil)
                                          nil))
                          (t rest-type)))))))))))

(defmethod meta-evaluate-instruction ((instruction cleavir-bir:fixed-to-multiple))
  (cleavir-bir:derive-type-for-linear-datum
   instruction
   (cleavir-ctype:values
    (mapcar #'cleavir-bir:ctype (cleavir-bir:inputs instruction))
    nil
    (cleavir-ctype:bottom nil)
    nil)))

(defmethod meta-evaluate-instruction ((instruction cleavir-bir:eq-test))
  (let ((inputs (cleavir-bir:inputs instruction)))
    (unless (constant-fold-instruction instruction inputs #'eq)
      ;; The tautology stuff doesn't help yet. We'd need EQL
      ;; constraints to actually do anything useful.
      #+(or)
      (let ((input1 (first inputs))
            (input2 (second inputs)))
        ;; (EQ <readvar <var X>> <readvar <var X>>) => T
        (when (and (typep input1 'cleavir-bir:readvar)
                   (typep input2 'cleavir-bir:readvar)
                   (eq (first (cleavir-bir:inputs input1))
                       (first (cleavir-bir:inputs input2))))
          #+(or)
          (print "folding tautalogy")
          (replace-computation-by-constant-value
           instruction
           t))
        ;; Do the transformation (if (eq <e> nil) <f> <g>) => (if <e> <g> <f>).
        ;; Really doesn't work yet.
        #+(or)
        (progn
          (when (typep input1 'cleavir-bir:constant-reference)
            (psetq input1 input2
                   input2 input1))
          (when (eq (cleavir-bir:constant-value (first (cleavir-bir:inputs input2))) nil)
            (let ((ifi (cleavir-bir:use instruction)))
              (assert (typep ifi 'cleavir-bir:ifi))
              (cleavir-bir::remove-use instruction ifi)
              (cleavir-bir:delete-computation instruction)
              (setf (cleavir-bir:inputs ifi) (list input1))
              (setf (cleavir-bir:next ifi) (nreverse (cleavir-bir:next ifi))))))))))

(defmethod meta-evaluate-instruction ((instruction cleavir-bir:typeq-test))
  (let* ((object (first (cleavir-bir:inputs instruction)))
         (ctype (cleavir-bir:ctype object))
         (type-specifier (cleavir-bir:type-specifier instruction)))
    (cond ((cleavir-ctype:subtypep ctype type-specifier nil)
           #+(or)
           (format t "~&folding typeq test ~a as true since testing ~a" type-specifier ctype)
           (replace-computation-by-constant-value
            instruction
            t))
          ;; XXX: Switch this to bottom-p when things are worked out
          ;; more.
          ((multiple-value-bind (disjoint certain)
               (cleavir-ctype:subtypep (cleavir-ctype:conjoin/2 ctype type-specifier nil)
                                       (cleavir-ctype:bottom nil)
                                       nil)
             (and disjoint certain))
           #+(or)
           (format t "~&folding typeq test ~a as false since testing ~a " type-specifier ctype)
           (replace-computation-by-constant-value
            instruction
            nil)))))

(defmethod meta-evaluate-instruction ((instruction cleavir-bir:leti))
  ;; For an immutable variable, we prove that the type of its readers
  ;; is just the type of its definition.
  (let ((variable (first (cleavir-bir:outputs instruction))))
    (when (cleavir-bir:immutablep variable)
      (let* ((definition (first (cleavir-bir:inputs instruction)))
             (type (cleavir-bir:ctype definition)))
        (cleavir-set:doset (reader (cleavir-bir:readers variable))
          (cleavir-bir:derive-type-for-linear-datum reader type))))))

(defmethod meta-evaluate-instruction ((instruction cleavir-bir:returni))
  ;; Propagate the return type to local calls and encloses of the function.
  (let ((function (cleavir-bir:function instruction))
        (return-type (cleavir-bir:ctype (first (cleavir-bir:inputs instruction)))))
    (cleavir-set:doset (local-call (cleavir-bir:local-calls function))
      (cleavir-bir:derive-type-for-linear-datum
       local-call
       return-type))
    (cleavir-set:doset (enclose (cleavir-bir:encloses function))
      (cleavir-bir:derive-type-for-linear-datum
       enclose
       (cleavir-ctype:function
        nil nil (cleavir-ctype:top nil) nil nil nil return-type nil)))))

(defmethod meta-evaluate-instruction ((instruction cleavir-bir:thei))
  ;; Remove THEI when its input's type is a subtype of the THEI's
  ;; asserted type.
  (let ((input (first (cleavir-bir:inputs instruction))))
    (when (cleavir-ctype:subtypep (cleavir-bir:ctype input)
                                  (cleavir-bir:asserted-type instruction)
                                  nil)
      (setf (cleavir-bir:inputs instruction) '())
      (cleavir-bir:replace-computation instruction input))))
