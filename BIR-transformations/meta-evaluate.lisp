;;;; The idea of meta-evaluation on BIR is that they are mostly
;;;; bottom-up and require no flow analysis, corresponding to
;;;; expression level optimizations and propagation on the original
;;;; expression tree. Loosely based on the design of ir1opt.lisp by
;;;; Rob MacLachlan in CMU CL.

(in-package #:cleavir-bir-transformations)

(defun meta-evaluate-module (module)
  #+(or)
  (cleavir-bir::print-disasm (cleavir-bir:disassemble module))
  (cleavir-set:doset (function (cleavir-bir:functions module))
    (meta-evaluate-function function)))

(defun meta-evaluate-function (function)
  ;; Obviously this should actually be a worklist algorithm and not
  ;; just two passes.
  (mapc
   #'meta-evaluate-iblock-forward
   (cleavir-bir::iblocks-forward-flow-order function))
  (cleavir-bir::map-iblocks-postorder
   #'meta-evaluate-iblock-backward
   function)
  (cleavir-bir:refresh-local-iblocks function))

;; 
(defun meta-evaluate-iblock-forward (iblock)
  ;; Make sure not to look at a block that might have been deleted
  ;; earlier in the forward pass.
  (unless (cleavir-bir:deletedp iblock)
    (cleavir-bir:do-iblock-instructions (instruction (cleavir-bir:start iblock))
      (meta-evaluate-instruction instruction))
    ;; Make sure to merge the successors as much as possible so we can
    ;; trigger more optimizations.
    (loop while (cleavir-bir:merge-successor-if-possible iblock))))

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
            (let ((name (cleavir-bir:name (cleavir-bir:info instruction))))
              (when (member name
                            '(fdefinition car cdr symbol-value))
                #+(or)
                (format t "~&meta-evaluate: flushing primop ~a" name)
                (cleavir-bir:delete-computation instruction))))))))))

(defgeneric meta-evaluate-instruction (instruction))

(defmethod meta-evaluate-instruction (instruction))

(defun constant-fold-eqi (instruction)
  (let* ((iblock (cleavir-bir:iblock instruction))
         (inputs (cleavir-bir:inputs instruction))
         (input1 (first inputs))
         (input2 (second inputs)))
    ;; If the arguments of EQI are both constant, then we can evaluate
    ;; the result at compile time and fold the correct branch.
    (when (and (typep input1 'cleavir-bir:constant-reference)
               (typep input2 'cleavir-bir:constant-reference))
      #+(or)
      (print "folding eqi instruction")
      (let* ((next (cleavir-bir:next instruction))
             (then (first next))
             (else (second next)))
        (multiple-value-bind (next dead)
            (if (eq (first (cleavir-bir:inputs input1))
                    (first (cleavir-bir:inputs input2)))
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
          (cleavir-bir:maybe-delete-iblock dead))))))

(defmethod meta-evaluate-instruction ((instruction cleavir-bir:eqi))
  (constant-fold-eqi instruction))
