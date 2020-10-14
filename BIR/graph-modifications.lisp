(in-package #:cleavir-bir)

(defmethod (setf dynamic-environment) :before
    ((nde dynamic-environment) (obj iblock))
  (cleavir-set:nremovef (scope (dynamic-environment obj)) obj)
  (cleavir-set:nadjoinf (scope nde) obj))

;;; Maintaining use and definition sets

(defgeneric remove-use (datum use))
(defmethod remove-use ((datum linear-datum) use)
  (declare (ignore use))
  (slot-makunbound datum '%use))
(defmethod remove-use ((datum variable) use)
  (cleavir-set:nremovef (cleavir-bir:readers datum) use))
(defmethod remove-use ((datum function) use)
  (cleavir-set:nremovef (local-calls datum) use))

(defgeneric add-use (datum use))
(defmethod add-use ((datum linear-datum) use)
  (assert (not (slot-boundp datum '%use))
          ()
          "Tried to add a use ~a to datum ~a, which is already in use by ~a"
          use datum (use datum))
  (setf (%use datum) use))
(defmethod add-use ((datum variable) use)
  (cleavir-set:nadjoinf (cleavir-bir:readers datum) use))
(defmethod add-use ((datum function) use)
  (cleavir-set:nadjoinf (local-calls datum) use))

(defmethod shared-initialize :before
    ((inst instruction) slot-names &rest initargs
     &key (inputs nil inputsp) &allow-other-keys)
  (declare (ignore slot-names initargs))
  ;; Maintain uses
  ;; The initform for inputs is nil, so we don't need to do this updating unless
  ;; an :inputs was provided.
  (when inputsp
    (when (slot-boundp inst '%inputs)
      (map nil (lambda (inp) (remove-use inp inst)) (inputs inst)))
    (map nil (lambda (inp) (add-use inp inst)) inputs)))

(defmethod (setf inputs) :before (new-inputs (inst instruction))
  (when (slot-boundp inst '%inputs)
    (map nil (lambda (inp) (remove-use inp inst)) (inputs inst)))
  (map nil (lambda (inp) (add-use inp inst)) new-inputs))

(defgeneric remove-definition (datum definition)
  (:method ((datum datum) (definition instruction))))
(defmethod remove-definition ((datum output) (definition instruction))
  (slot-makunbound datum '%definition))

(defgeneric add-definition (datum definition)
  (:method ((datum datum) (definition instruction))))
(defmethod add-definition ((datum output) (definition instruction))
  (assert (not (slot-boundp datum '%definition)))
  (setf (%definition datum) definition))
(defmethod add-definition ((datum variable) (definition instruction))
  (cleavir-set:nadjoinf (writers datum) definition))

(defmethod shared-initialize :before
    ((inst operation) slot-names &rest initargs &key outputs &allow-other-keys)
  (declare (ignore initargs))
  ;; Maintain use lists
  (when (or (eq slot-names 't) (member '%outputs slot-names))
    (when (slot-boundp inst '%outputs)
      (map nil (lambda (outp) (remove-definition outp inst)) (outputs inst)))
    (map nil (lambda (outp) (add-definition outp inst)) outputs)))

(defmethod (setf outputs) :before (new-outputs (inst operation))
  (when (slot-boundp inst '%outputs)
    (map nil (lambda (outp) (remove-definition outp inst)) (outputs inst)))
  (map nil (lambda (outp) (add-definition outp inst)) new-outputs))

;;; Control flow modification

(defun insert-instruction-before (new existing)
  (let ((pred (predecessor existing))
        (ib (iblock existing)))
    (setf (predecessor existing) new
          (successor new) existing (predecessor new) pred
          (iblock new) ib)
    (if pred
        (setf (successor pred) new)
        (setf (start ib) new)))
  (values))

(defun insert-instruction-after (new existing)
  (check-type existing (and instruction (not terminator)))
  (let ((succ (successor existing)))
    (setf (predecessor succ) new (successor existing) new
          (predecessor new) existing (successor new) succ))
  (values))

;;; Remove backpointers to an instruction, etc.
(defgeneric clean-up-instruction (instruction)
  (:method-combination progn)
  (:method progn ((instruction instruction))
    (when (slot-boundp instruction '%inputs)
      (dolist (in (inputs instruction))
        (remove-use in instruction)))))

(defgeneric clean-up-iblock (iblock)
  (:method-combination progn)
  (:method progn ((ib iblock))
    (cleavir-set:nremovef (iblocks (function ib)) ib)
    (cleavir-set:nremovef (scope (dynamic-environment ib)) ib)
    ;; NOTE: clean-up on the terminator disconnects predecessors
    (when (slot-boundp ib '%start)
      (map-iblock-instructions #'clean-up-instruction (start ib)))))

(defgeneric remove-binding (variable binder)
  (:method (variable binder) (declare (ignore variable binder))))
(defmethod remove-binding (variable (binder leti))
  (let ((bindings (bindings binder)))
    (cleavir-set:nremovef bindings variable)
    (when (cleavir-set:empty-set-p (bindings binder))
      (cleavir-bir:delete-instruction binder))))

;;; If a variable is no longer referenced, remove it from its function
;;; and binder if possible.
(defun maybe-clear-variable (variable)
  (when (and (cleavir-set:empty-set-p (cleavir-bir:readers variable))
             (cleavir-set:empty-set-p (cleavir-bir:writers variable)))
    (cleavir-set:nremovef (cleavir-bir:variables (function variable)) variable)
    (remove-binding variable (binder variable))
    t))

(defmethod clean-up-instruction progn ((inst readvar))
  (let ((variable (first (inputs inst))))
    (cleavir-set:nremovef (readers variable) inst)
    (maybe-clear-variable variable)))
(defmethod clean-up-instruction progn ((inst writevar))
  (let ((variable (first (outputs inst))))
    (cleavir-set:nremovef (writers variable) inst)
    (maybe-clear-variable variable)))
(defmethod clean-up-instruction progn ((inst enclose))
  (let* ((code (code inst))
         (code-encloses (encloses code)))
    (cleavir-set:nremovef code-encloses inst)))
(defmethod clean-up-instruction progn ((inst unwind))
  (cleavir-set:nremovef (entrances (destination inst)) (iblock inst))
  (cleavir-set:nremovef (unwinds (catch inst)) inst))
(defmethod clean-up-instruction progn ((inst catch))
  (let ((variable (first (outputs inst))))
    (cleavir-set:nremovef (writers variable) inst)
    (maybe-clear-variable variable)))
(defmethod clean-up-instruction progn ((inst terminator))
  (let ((ib (iblock inst)))
    (dolist (n (next inst)) (cleavir-set:nremovef (predecessors n) ib))))

;;; Delete an instruction. Must not be a terminator.
(defun delete-instruction (instruction)
  (check-type instruction (and instruction (not terminator)))
  (typecase instruction
    (computation (assert (unused-p instruction)))
    (operation
     (assert (every (lambda (o) (or (not (ssa-p o)) (unused-p o)))
                    (outputs instruction)))))
  (clean-up-instruction instruction)
  ;; Delete from the control flow.
  (let ((pred (predecessor instruction))
        (succ (successor instruction)))
    (assert (not (null succ)))
    (setf (predecessor succ) pred)
    (cond ((null pred)
           ;; We start a block, so we need to change the iblock's start.
           (setf (start (iblock instruction)) succ))
          (t
           (setf (successor pred) succ))))
  (values))

(defgeneric replace-terminator (new old))

(defmethod replace-terminator ((new terminator) (old terminator))
  (let ((ib (iblock old))
        (new-next (next new))
        (pred (predecessor old)))
    (clean-up-instruction old)
    (if pred
        (setf (successor pred) new)
        ;; this block has only one instruction - the terminator.
        (setf (start ib) new))
    (setf (predecessor new) pred
          (end ib) new
          (iblock new) ib)
    (dolist (n (next old))
      (cleavir-set:nremovef (predecessors n) ib))
    (dolist (n new-next)
      (cleavir-set:nadjoinf (predecessors n) ib)))
  (values))

(defmethod replace-terminator :after ((new unwind) old)
  (cleavir-set:nadjoinf (entrances (destination new)) (iblock new)))

(defun delete-iblock (iblock)
  ;; FIXME: Should note reasons to the user if nontrivial code is being
  ;; deleted. Or perhaps that should be handled at a higher level?
  (assert (and (cleavir-set:empty-set-p (predecessors iblock))
               (cleavir-set:empty-set-p (entrances iblock))))
  (clean-up-iblock iblock)
  (let ((f (function iblock)))
    (when (and (slot-boundp f '%end)
               (eq iblock (end f)))
      (setf (end f) nil)))
  (when (slot-boundp iblock '%end)
    (let ((successors (successors iblock)))
      (dolist (s successors)
        (cleavir-set:nremovef (predecessors s) iblock)
        (when (cleavir-set:empty-set-p (predecessors s))
          (delete-iblock s))))))

(defun maybe-delete-iblock (iblock)
  (when (and (cleavir-set:empty-set-p (predecessors iblock))
             (cleavir-set:empty-set-p (entrances iblock)))
    (delete-iblock iblock)))

;;; Internal. Replace one value with another in an input list.
(defun replace-input (new old instruction)
  (check-type instruction instruction)
  (setf (inputs instruction)
        (nsubstitute new old (inputs instruction) :test #'eq)))

(defgeneric replace-uses (new old))
(defmethod replace-uses ((new datum) (old datum))
  (cleavir-set:doset (use (uses old))
    (replace-input new old use)))
(defmethod replace-uses ((new linear-datum) (old linear-datum))
  (assert (not (slot-boundp new '%use)))
  (when (slot-boundp old '%use)
    (setf (%use new) (%use old))
    (replace-input new old (%use old)))
  (values))
(defmethod replace-uses :after ((new datum) (old linear-datum))
  (when (slot-boundp old '%use)
    (slot-makunbound old '%use)))

;;; Delete a computation, replacing its use with the given LINEAR-DATUM.
(defun replace-computation (computation replacement)
  (replace-uses replacement computation)
  (delete-instruction computation)
  (values))

;;; Delete a computation with unused result.
(defun delete-computation (computation)
  (check-type computation computation)
  (assert (unused-p computation))
  (delete-instruction computation)
  (values))

;;; Deletes a pair of instructions that pass values along.
;;; That is, given inputs -> in-inst ... out-inst -> outputs,
;;; we replace the outputs with the inputs and delete both instructions.
;;; This is a separate function because for one thing it's reasonably common,
;;; and for two it internally messes with several invariants of the IR, which
;;; would be tricky to deal with outside of the BIR system.
(defun delete-transmission (in-inst out-inst)
  (let ((outputs (outputs out-inst))
        (inputs (inputs in-inst)))
    (assert (every #'ssa-p outputs))
    (assert (>= (length inputs) (length outputs)))
    ;; Prevent it from cleaning up its inputs when it's deleted.
    (slot-makunbound in-inst '%inputs)
    ;; Clean up its inputs.
    ;; (This is a separate loop in case inputs is longer than outputs.)
    (loop for inp in inputs do (remove-use inp in-inst))
    ;; Replace.
    (mapc #'replace-uses inputs outputs))
  (delete-instruction out-inst)
  (delete-instruction in-inst))

(defun iblocks-mergable-p (iblock1 iblock2)
  (let ((predecessors (predecessors iblock2)))
    (and (typep (cleavir-bir:end iblock1)
                'cleavir-bir:jump)
         (eq (first (successors iblock1)) iblock2)
         (= (cleavir-set:size predecessors) 1)
         (cleavir-set:empty-set-p (entrances iblock2))
         (eq (cleavir-set:arb predecessors) iblock1)
         (eq (function iblock1) (function iblock2))
         (eq (dynamic-environment iblock1)
             (dynamic-environment iblock2))
         ;; Infinite loop.
         (not (eq iblock1 iblock2)))))

;;; Merge two iblocks, the first of which must end in a jump.
(defun merge-iblocks (iblock1 iblock2)
  (check-type iblock1 iblock)
  (check-type iblock2 iblock)
  (assert (iblocks-mergable-p iblock1 iblock2))
  (let ((end-predecessor (predecessor (end iblock1)))
        (start (start iblock2))
        (function (function iblock2)))
    (cond (end-predecessor
           (setf (successor end-predecessor) start)
           (setf (predecessor start) end-predecessor))
          (t
           (setf (start iblock1) start)))
    (setf (end iblock1) (end iblock2))
    (map-iblock-instructions
     (lambda (instruction)
       (setf (cleavir-bir:iblock instruction)
             iblock1))
     start)
    ;; Update the predecessors of the successors.
    (dolist (succ (successors iblock2))
      (cleavir-set:nremovef (predecessors succ) iblock2)
      (cleavir-set:nadjoinf (predecessors succ) iblock1))
    ;; If the block happens to be the end of its function, adjust
    (when (eq (end function) iblock2)
      (setf (end function) iblock1))
    ;; Remove iblock2 from the function.
    (cleavir-set:nremovef (iblocks function) iblock2)
    ;; and scope
    (cleavir-set:nadjoinf (scope (dynamic-environment iblock2)) iblock2)
    iblock1))

;;; Split a iblock into two iblocks.
(defun split-block-after (inst)
  (check-type inst (and instruction (not terminator)))
  ;; the new block is the block after, because there's a little less to update.
  (let* ((ib (iblock inst))
         (function (function ib))
         (new (make-instance 'iblock
                :function (function ib) :inputs nil
                :predecessors (cleavir-set:make-set ib)
                :dynamic-environment (dynamic-environment ib)))
         (new-start (successor inst)))
    ;; Add the new block to the function
    (cleavir-set:nadjoinf (iblocks function) new)
    ;; and scope
    (cleavir-set:nadjoinf (scope (dynamic-environment ib)) new)
    ;; Set the new start to lose its predecessor
    (setf (predecessor new-start) nil)
    ;; Move the later instructions
    (setf (start new) new-start (end new) (end ib))
    (loop for i = new-start then (successor i)
          until (null i)
          do (setf (iblock i) new))
    ;; Put a new terminator in the before block
    (let ((new (make-instance 'jump
                 :iblock ib :inputs () :predecessor inst
                 :next (list new))))
      (setf (successor inst) new (end ib) new))
    (let ((end (end new)))
      (if (typep end 'unwind)
          ;; Update the new block's presence in entrances
          (let ((dest (destination end)))
            (cleavir-set:nremovef (entrances dest) ib)
            (cleavir-set:nadjoinf (entrances dest) new))
          ;; or predecessors
          (dolist (n (next (end new)))
            (cleavir-set:nremovef (predecessors n) ib)
            (cleavir-set:nadjoinf (predecessors n) new))))
    ;; If the block happens to be the end of its function, adjust
    (when (eq (end function) ib)
      (setf (end function) new))
    (values ib new)))

(defun reachable-iblocks (function)
  (check-type function function)
  (let ((set (cleavir-set:empty-set))
        (worklist (list (start function))))
    (loop for work = (pop worklist)
          until (null work)
          unless (cleavir-set:presentp work set)
            do (cleavir-set:nadjoinf set work)
               (setf worklist (append (next (end work)) worklist)))
    set))

;;; make the iblocks field match the actually reachable blocks.
(defun refresh-local-iblocks (function)
  (check-type function function)
  (setf (iblocks function) (reachable-iblocks function)))

(defun refresh-iblocks (module)
  (cleavir-set:mapset nil #'refresh-local-iblocks (functions module)))

(defun refresh-local-users (function)
  (check-type function function)
  ;;; First zero out existing uses
  (map-instructions
   (lambda (inst)
     (dolist (input (inputs inst))
       (remove-use input inst)))
   function)
  ;;; Now add em back
  (map-instructions
   (lambda (inst)
     (dolist (input (inputs inst))
       (add-use input inst)))
   function)
  (values))

(defun refresh-users (top)
  (cleavir-set:mapset nil #'refresh-local-users (functions (module top))))

(defun remove-function-from-module (function)
  (cleavir-set:nremovef (cleavir-bir:functions (cleavir-bir:module function))
                        function))
