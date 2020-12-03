;;;; The idea of meta-evaluation on BIR is that they are mostly
;;;; bottom-up and require no flow analysis, corresponding to
;;;; expression level optimizations and propagation on the original
;;;; expression tree. Loosely based on the design of ir1opt.lisp by
;;;; Rob MacLachlan in CMU CL.

(in-package #:cleavir-bir-transformations)

(defun meta-evaluate-module (module)
  (cleavir-set:doset (function (cleavir-bir:functions module))
    (meta-evaluate-function function)))

(defun meta-evaluate-function (function)
  ;; Obviously this should actually be a worklist algorithm and not
  ;; just two or three passes.
  (let ((forward-flow (cleavir-bir::iblocks-forward-flow-order function)))
    (dotimes (repeat 3)
      (declare (ignore repeat))
      (dolist (iblock forward-flow)
        ;; Make sure not to look at a block that might have been
        ;; deleted earlier in this forward pass.
        (unless (cleavir-bir:deletedp iblock)
          ;; Make sure to merge the successors as much as possible so we can
          ;; trigger more optimizations.
          (loop while (cleavir-bir:merge-successor-if-possible iblock))
          (meta-evaluate-iblock-forward iblock)))))
  (cleavir-bir:refresh-local-iblocks function)
  (cleavir-bir::map-iblocks-postorder
   #'meta-evaluate-iblock-backward
   function))

;; 
(defun meta-evaluate-iblock-forward (iblock)
  (cleavir-bir:do-iblock-instructions (instruction (cleavir-bir:start iblock))
    (meta-evaluate-instruction instruction)))

;; Remove dead code for the backward pass.
(defun meta-evaluate-iblock-backward (iblock)
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

(defun constant-fold-ifi (instruction)
  (let* ((iblock (cleavir-bir:iblock instruction))
         (test (first (cleavir-bir:inputs instruction))))
    ;; If the test input of IFI is constant, then we can immediately
    ;; fold to the correct branch.
    (when (typep test 'cleavir-bir:constant-reference)
      #+(or)
      (print "folding ifi instruction")
      (let* ((next (cleavir-bir:next instruction))
             (then (first next))
             (else (second next)))
        (multiple-value-bind (next dead)
            (if (cleavir-bir:constant-value (first (cleavir-bir:inputs test)))
                (values then else)
                (values else then))
          (cleavir-bir:replace-terminator
           (make-instance 'cleavir-bir:jump
                          :next (list next)
                          :inputs '() :outputs '())
           instruction)
          ;; Try to delete the block if possible, so we can maybe
          ;; optimize more in this pass. Ultimately,
          ;; refresh-local-iblocks is supposed to flush dead blocks.
          (cleavir-bir:maybe-delete-iblock dead)))
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
  (unless (constant-fold-ifi instruction)
    (eliminate-degenerate-if instruction)))

;; Replace COMPUTATION with a constant reference to value.
(defun replace-computation-by-constant-value (instruction value)
  (cleavir-bir:replace-computation
   instruction
   (cleavir-bir:make-constant-reference
    (cleavir-bir:constant-in-module
     value
     (cleavir-bir:module (cleavir-bir:function instruction))))))

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
    (when (typep definition 'cleavir-bir:fixed-to-multiple)
      (cleavir-bir:delete-transmission definition instruction)
      (cleavir-bir:delete-instruction definition))))

(defmethod meta-evaluate-instruction ((instruction cleavir-bir:eq-test))
  (let ((inputs (cleavir-bir:inputs instruction)))
    (unless (constant-fold-instruction instruction inputs #'eq)
      ;; Really doesn't work yet.
      #+(or)
      (let ((input1 (first inputs))
            (input2 (second inputs)))
        ;; Do the transformation (if (eq <e> nil) <f> <g>) => (if <e> <g> <f>).
        (when (typep input1 'cleavir-bir:constant-reference)
          (psetq input1 input2
                 input2 input1))
        (when (eq (cleavir-bir:constant-value (first (cleavir-bir:inputs input2))) nil)
          (let ((ifi (cleavir-bir:use instruction)))
            (assert (typep ifi 'cleavir-bir:ifi))
            (cleavir-bir::remove-use instruction ifi)
            (cleavir-bir:delete-computation instruction)
            (setf (cleavir-bir:inputs ifi) (list input1))
            (setf (cleavir-bir:next ifi) (nreverse (cleavir-bir:next ifi)))))))))

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
