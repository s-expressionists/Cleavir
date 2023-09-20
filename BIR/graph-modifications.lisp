(in-package #:cleavir-bir)

(defmethod (setf dynamic-environment) :before
    ((nde dynamic-environment) (obj iblock))
  (when (slot-boundp obj '%dynamic-environment)
    (set:nremovef (scope (dynamic-environment obj)) obj))
  (set:nadjoinf (scope nde) obj))

;;; Maintaining use and definition sets

(defgeneric remove-use (datum use))
(defmethod remove-use ((datum linear-datum) use)
  (when (eql (%use datum) use)
    (setf (%use datum) nil)))
(defmethod remove-use ((datum variable) use)
  (set:nremovef (readers datum) use))
(defmethod remove-use ((datum constant) use)
  (set:nremovef (readers datum) use))
(defmethod remove-use ((datum load-time-value) use)
  (set:nremovef (readers datum) use))
(Defmethod remove-use ((datum function-cell) use)
  (set:nremovef (readers datum) use))
(Defmethod remove-use ((datum variable-cell) use)
  (set:nremovef (readers datum) use))
(defmethod remove-use ((datum function) (use abstract-local-call))
  (set:nremovef (local-calls datum) use))
(defmethod remove-use ((datum function) (use thei))
  (set:nremovef (other-uses datum) use))

(defgeneric add-use (datum use))
(defmethod add-use ((datum linear-datum) use)
  ;; Use of this and other methods (e.g. add-definition) can render the IR
  ;; inconsistent. For example, if we have (f x) and insert (g x), the f
  ;; instruction will no longer use x, though it has it as an input.
  ;; The inconsistency should be temporary, as can be accomplished for
  ;; example by deleting f. Use the verifier judiciously.
  (setf (%use datum) use))
(defmethod add-use ((datum variable) use)
  (set:nadjoinf (readers datum) use))
(defmethod add-use ((datum constant) use)
  (set:nadjoinf (readers datum) use))
(defmethod add-use ((datum load-time-value) use)
  (set:nadjoinf (readers datum) use))
(defmethod add-use ((datum function-cell) use)
  (set:nadjoinf (readers datum) use))
(defmethod add-use ((datum variable-cell) use)
  (set:nadjoinf (readers datum) use))
(defmethod add-use ((datum function) (use abstract-local-call))
  (set:nadjoinf (local-calls datum) use))
(defmethod add-use ((datum function) (use thei))
  (set:nadjoinf (other-uses datum) use))

(defmethod (setf inputs) :before (new-inputs (inst instruction))
  (dolist (input (inputs inst))
    (remove-use input inst))
  (dolist (input new-inputs)
    (add-use input inst)))

(defgeneric remove-definition (datum definition)
  (:method ((datum datum) (definition instruction))))
(defmethod remove-definition ((datum output) (definition instruction))
  (when (eql (definition datum) definition)
    (setf (%definition datum) nil)))

(defgeneric add-definition (datum definition)
  (:method ((datum datum) (definition instruction))))
(defmethod add-definition ((datum output) (definition instruction))
  (setf (%definition datum) definition))
(defmethod add-definition ((datum variable) (definition instruction))
  (set:nadjoinf (writers datum) definition))

(defmethod (setf outputs) :before (new-outputs (inst instruction))
  (dolist (output (outputs inst))
    (remove-definition output inst))
  (dolist (output new-outputs)
    (add-definition output inst)))

(defmethod shared-initialize :before
    ((inst instruction) slot-names &rest initargs
     &key (inputs nil inputsp) (outputs nil outputsp))
  (declare (cl:ignore slot-names initargs))
  ;; Maintain uses
  (when inputsp
    (when (slot-boundp inst '%inputs)
      (dolist (input (inputs inst))
        (remove-use input inst)))
    (dolist (input inputs)
      (add-use input inst)))
  (when outputsp
    (when (slot-boundp inst '%outputs)
      (dolist (output (outputs inst))
        (remove-definition output inst)))
    (dolist (output outputs)
      (add-definition output inst))))

(defmethod shared-initialize :before
    ((inst thei) slot-names &rest initargs
     &key (type-check-function nil tcfp))
  (declare (cl:ignore slot-names initargs))
  (when tcfp
    (when (slot-boundp inst '%type-check-function)
      (let ((old-tcf (type-check-function inst)))
        (unless (symbolp old-tcf)
          (remove-use old-tcf inst))))
    (when (typep type-check-function 'function)
      (add-use type-check-function inst))))

(defmethod (setf type-check-function) :before (tcf (inst thei))
  (let ((old-tcf (type-check-function inst)))
    (unless (symbolp old-tcf)
      (remove-use old-tcf inst)))
  (when (typep tcf 'function)
    (add-use tcf inst)))

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
  "Insert a NEW instruction immediatley after an EXISTING instruction, which must not be a terminator."
  (check-type existing (and instruction (not terminator)))
  (let ((succ (successor existing)))
    (setf (predecessor succ) new (successor existing) new
          (predecessor new) existing (successor new) succ
          (iblock new) (iblock existing)))
  (values))

(defun move-instruction-before (movant existing)
  (assert (not (eql movant existing)))
  (unlink-instruction movant)
  (insert-instruction-before movant existing))

(defun move-instruction-after (movant existing)
  (assert (not (eql movant existing)))
  (unlink-instruction movant)
  (insert-instruction-after movant existing))

;;; Remove backpointers to an instruction, etc.
(defgeneric clean-up-instruction (instruction)
  (:method-combination progn)
  (:method progn ((instruction instruction))
    (dolist (in (inputs instruction))
      (remove-use in instruction))))

;;; FIXME: Should this be exported?
(defgeneric clean-up-iblock (iblock)
  (:method-combination progn)
  (:method progn ((ib iblock))
    (set:nremovef (scope (dynamic-environment ib)) ib)
    ;; NOTE: clean-up on the terminator disconnects predecessors
    (when (slot-boundp ib '%start)
      (map-iblock-instructions #'clean-up-instruction ib)))
  (:documentation "Clean up links to IBLOCK without removing it from control flow."))

(defgeneric clean-up-function (function)
  (:method-combination progn)
  (:method progn ((function function))
    (set:nremovef (functions (module function)) function)
    (map-iblocks #'clean-up-iblock function)))

;;; Remove a variable from a module.
(defun clean-up-variable (variable)
  (set:nremovef (variables (function variable)) variable)
  ;; Flame about source variables that were never used.

  ;; FIXME: We need to flame better than this, for example by
  ;; propagating source information and displaying the form of the
  ;; unique writer, and also making this warning a subclass of
  ;; style-warning, as mandated by the spec. Also make sure this
  ;; warning only happens for source variables, with coordination with
  ;; CSTs, for example.
  (unless (or (ignore variable)
              (eq (use-status variable) 'read))
    (warn 'unused-variable :variable variable :origin (origin (binder variable)))))

(defmethod clean-up-instruction progn ((inst readvar))
  (let ((variable (first (inputs inst))))
    (set:nremovef (readers variable) inst)
    ;; If a variable is no longer referenced, remove all of its
    ;; writers.
    (when (set:empty-set-p (readers variable))
      (set:doset (writer (writers variable))
        (delete-instruction writer)))))
(defmethod clean-up-instruction progn ((inst writevar))
  (let ((variable (output inst)))
    (set:nremovef (writers variable) inst)
    ;; When the variable no longer has any writers or readers, clean it
    ;; up.
    (when (and (set:empty-set-p (writers variable))
               (set:empty-set-p (readers variable)))
      (clean-up-variable variable))))
(defmethod clean-up-instruction progn ((inst constant-reference))
  (let ((constant (first (inputs inst))))
    (set:nremovef (readers constant) inst)
    (when (set:empty-set-p (readers constant))
      (let ((module (module (function inst))))
        (set:nremovef (constants module) constant)
        (remhash (constant-value constant) (constant-table module))))))
(defmethod clean-up-instruction progn ((inst constant-fdefinition))
  (let ((constant (first (inputs inst))))
    (set:nremovef (readers constant) inst)
    (when (set:empty-set-p (readers constant))
      (let ((module (module (function inst))))
        (set:nremovef (constants module) constant)
        (remhash (function-name constant) (function-cell-table module))))))
(defmethod clean-up-instruction progn ((inst constant-symbol-value))
  (let ((constant (first (inputs inst))))
    (set:nremovef (readers constant) inst)
    (when (set:empty-set-p (readers constant))
      (let ((module (module (function inst))))
        (set:nremovef (constants module) constant)
        (remhash (variable-name constant) (variable-cell-table module))))))
(defmethod clean-up-instruction progn ((inst set-constant-symbol-value))
  (let ((constant (first (inputs inst))))
    (set:nremovef (readers constant) inst)
    (when (set:empty-set-p (readers constant))
      (let ((module (module (function inst))))
        (set:nremovef (constants module) constant)
        (remhash (variable-name constant) (variable-cell-table module))))))
(defmethod clean-up-instruction progn ((inst constant-bind))
  (let ((constant (first (inputs inst))))
    (set:nremovef (readers constant) inst)
    (when (set:empty-set-p (readers constant))
      (let ((module (module (function inst))))
        (set:nremovef (constants module) constant)
        (remhash (variable-name constant) (variable-cell-table module))))))
(defmethod clean-up-instruction progn ((inst load-time-value-reference))
  (let ((ltv (first (inputs inst))))
    (set:nremovef (readers ltv) inst)
    (when (set:empty-set-p (readers ltv))
      (set:nremovef (constants (module (function inst))) ltv))))
(defmethod clean-up-instruction progn ((inst enclose))
  (let ((code (code inst)))
    (setf (enclose code) nil)
    (when (set:empty-set-p (local-calls code))
      (clean-up-function code))))
(defmethod clean-up-instruction progn ((inst abstract-local-call))
  (let* ((code (callee inst))
         (local-calls (local-calls code)))
    (set:nremovef local-calls inst)
    (when (and (null (enclose code))
               (set:empty-set-p local-calls))
      (clean-up-function code))))
(defmethod clean-up-instruction progn ((inst unwind))
  (set:nremovef (entrances (destination inst)) (iblock inst))
  (set:nremovef (unwinds (come-from inst)) inst))
(defmethod clean-up-instruction progn ((inst come-from))
  (set:nremovef (come-froms (function inst)) inst))
(defmethod clean-up-instruction progn ((inst terminator))
  (let ((ib (iblock inst)))
    (dolist (n (next inst)) (set:nremovef (predecessors n) ib))))
(defmethod clean-up-instruction progn ((inst returni))
  (setf (returni (function inst)) nil))
(defmethod clean-up-instruction progn ((inst thei))
  (let ((type-check-function (type-check-function inst)))
    (unless (symbolp type-check-function)
      (remove-use type-check-function inst)
      (assert (and (null (enclose type-check-function))
                   (set:empty-set-p (local-calls type-check-function))
                   (set:empty-set-p (other-uses type-check-function)))
              ()
              "Type check function for THEI should not have local calls or enclose or other uses!")
      (clean-up-function type-check-function))))

(defun delete-thei (thei)
  "Remove a THEI by forwarding its input to its use."
  (replace-uses (input thei) (output thei))
  (delete-instruction thei))

;;; Return a copy of a list containing all but the indexed element.
(defun list-sans-index (list index)
  (loop for elem in list for p from 0 unless (= index p) collect elem))

;;; FIXME: Maybe this shouldn't be exported - there's no check if the phi is actually unused
(defun delete-phi (phi)
  (let ((iblock (iblock phi)))
    (setf (inputs iblock)
          (delete phi (inputs iblock)))
    (set:doset (def (definitions phi))
      (let ((pos (position phi (outputs def))))
        (setf (outputs def) (list-sans-index (outputs def) pos))
        (setf (inputs def) (list-sans-index (inputs def) pos))))))

;;; Remove an instruction from control flow without cleaning it up.
;;; This can be used to move instructions.
(defun unlink-instruction (instruction)
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

(defun delete-instruction (instruction)
  "Delete an instruction. It must not be a terminator."
  (check-type instruction (and instruction (not terminator)))
  (clean-up-instruction instruction)
  (unlink-instruction instruction)
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
      (set:nremovef (predecessors n) ib))
    (dolist (n new-next)
      (set:nadjoinf (predecessors n) ib)))
  (values))

;; Update the iblocks in scope of a replaced terminator.
(defmethod replace-terminator :before (new (old dynamic-environment))
  ;; We use TYPEP instead of specialized methods because we need only
  ;; one scope update, which would be difficult to arrange with two methods.
  (let ((nde (if (typep new 'dynamic-environment)
                 new
                 (dynamic-environment old))))
    (set:doset (s (scope old))
      (setf (dynamic-environment s) nde))))

(defmethod replace-terminator :after ((new unwind) old)
  (declare (cl:ignore old))
  (set:nadjoinf (entrances (destination new)) (iblock new)))

(defmethod replace-terminator :after (new (old unwind))
  (declare (cl:ignore new))
  (set:nremovef (entrances (destination old)) (iblock old)))

(defun orphan-iblock-p (iblock)
  (and (set:empty-set-p (predecessors iblock))
       (set:empty-set-p (entrances iblock))))

(defun delete-iblock (iblock)
  "Delete an iblock from the program. The iblock must not be reachable.

See MAYBE-DELETE-IBLOCK"
  ;; FIXME: Should note reasons to the user if nontrivial code is being
  ;; deleted. Or perhaps that should be handled at a higher level?
  (assert (orphan-iblock-p iblock))
  (clean-up-iblock iblock)
  (when (slot-boundp iblock '%end)
    (let ((successors (successors iblock)))
      (setf (next (end iblock)) nil) ; prevent looping
      (dolist (s successors)
        (set:nremovef (predecessors s) iblock)
        (maybe-delete-iblock s))))
  (remove-iblock-from-flow-order iblock))

(defun maybe-delete-iblock (iblock)
  "If IBLOCK is unreachable, delete it."
  (when (orphan-iblock-p iblock)
    (delete-iblock iblock)))

;;; Internal. Replace one value with another in an input list.
(defun replace-input (new old instruction)
  (check-type instruction instruction)
  (setf (inputs instruction)
        (nsubstitute new old (inputs instruction) :test #'eq)))

(defgeneric replace-uses (new old))
(defmethod replace-uses ((new datum) (old linear-datum))
  (replace-input new old (use old)))
(defmethod replace-uses ((new linear-datum) (old linear-datum))
  (when (use old)
    (setf (%use new) (%use old))
    (replace-input new old (%use old)))
  (values))
(defmethod replace-uses :after ((new datum) (old linear-datum))
  (when (use old)
    (setf (%use old) nil)))

;;; Remove IBLOCK from its flow ordering.
(defun remove-iblock-from-flow-order (iblock)
  (let ((prev (%prev iblock))
        (next (%next iblock)))
    ;; Clasp bug #1260 demonstrates a situation in which we accidentally
    ;; called this function on an iblock which was not in fact in the flow
    ;; order to begin with. These asserts check that it is.
    ;; ...but unfortunately, we can't use them, because they're triggered
    ;; by some correct code duplications as well, such as when delete-iblock
    ;; recurses into a loop. I have tried a few simple deletion checks but
    ;; they failed for various reasons (e.g. variable deletion can implicate
    ;; iblocks that have already been deleted, somehow). FIXME.
    #+(or)
    (assert (eq (%next prev) iblock))
    (when prev
      (setf (%next prev) next))
    (cond (next
           #+(or)
           (assert (eq (%prev next) iblock))
           (setf (%prev next) prev))
          (t (setf (tail (function iblock)) prev)))))

;;; Insert the new iblock into the flow order after AFTER.
(defun insert-iblock-into-flow-order (new after)
  (let ((next (%next after)))
    (setf (%next after) new)
    (setf (%prev new) after)
    (setf (%next new) next)
    (if next
        (setf (%prev next) new)
        (setf (tail (function after)) new))))

(defun merge-successor-if-possible (iblock)
  "Merge IBLOCK to its unique successor if possible.
If it is possible, return true, otherwise false."
  (when (successor-mergable-p iblock)
    (merge-successor iblock)))

(defun successor-mergable-p (iblock)
  (let ((successors (successors iblock)))
    (and successors
         (typep (end iblock) 'jump)
         (let* ((successor (first successors))
                (predecessors (predecessors successor)))
           (and (= (set:size predecessors) 1)
                (set:empty-set-p (entrances successor))
                (eq (function iblock) (function successor))
                (eq (dynamic-environment iblock)
                    (dynamic-environment successor))
                ;; Infinite loop.
                (not (eq iblock successor)))))))

;;; Merge an iblock ending in a jump with its successor. Also forward
;;; the jumps outputs to the successor block's inputs.
(defun merge-successor (iblock)
  (let* ((jump (end iblock))
         (successor (first (successors iblock)))
         (end-predecessor (predecessor jump))
         (start (start successor))
         (end (end successor)))
    (cond (end-predecessor
           (setf (successor end-predecessor) start)
           (setf (predecessor start) end-predecessor))
          (t
           (setf (start iblock) start)))
    (setf (end iblock) end)
    (do-iblock-instructions (instruction iblock)
      (setf (iblock instruction) iblock))
    ;; Propagate the inputs of the jump into the uses of the second
    ;; block's phis.
    (mapc (lambda (input phi)
            (remove-use input jump)
            (replace-uses input phi))
          (inputs jump) (inputs successor))
    (if (typep end 'unwind)
        ;; Update the new block's presence in entrances
        (let ((dest (destination end)))
          (set:nremovef (entrances dest) successor)
          (set:nadjoinf (entrances dest) iblock))
        ;; Update the predecessors of the successors.
        (dolist (succ (successors successor))
          (set:nremovef (predecessors succ) successor)
          (set:nadjoinf (predecessors succ) iblock)))
    (remove-iblock-from-flow-order successor)
    ;; Delete from scope.
    (set:nremovef (scope (dynamic-environment successor)) successor)
    ;; The successor block is now conceptually deleted.
    (setf (function successor) nil)
    iblock))

(defun empty-iblock-p (iblock)
  (let ((start (start iblock)))
    (and (typep start 'jump)
         (not (unwindp start))
         (null (inputs iblock))
         (null (outputs start))
         (not (eq (start (function iblock)) iblock))
         (not (eq iblock (first (next start))))
         (set:empty-set-p (entrances iblock)))))

(defun delete-iblock-if-empty (iblock)
  "If IBLOCK is empty (i.e. consists entirely of a JUMP instruction and is not nonlocally exited to), forward its predecessors to its successors."
  (when (empty-iblock-p iblock)
    (let ((successor (first (successors iblock)))
          (predecessors (predecessors iblock)))
      (set:doset (predecessor predecessors)
        (let ((end (end predecessor)))
          (nsubstitute successor iblock (next end)))
        (set:nadjoinf (predecessors successor) predecessor))
      (set:nremovef (predecessors successor) iblock))
    (remove-iblock-from-flow-order iblock)
    ;; Remove from scope.
    (set:nremovef (scope (dynamic-environment iblock)) iblock)
    iblock))

;;; Split a iblock into two iblocks.
(defun split-block-after (inst)
  (check-type inst (and instruction (not terminator)))
  ;; the new block is the block after, because there's a little less to update.
  (let* ((ib (iblock inst))
         (new (make-instance 'iblock
                :function (function ib) :inputs nil
                :predecessors (set:make-set ib)
                :dynamic-environment (dynamic-environment ib)))
         (new-start (successor inst)))
    (insert-iblock-into-flow-order new ib)
    ;; and scope
    (set:nadjoinf (scope (dynamic-environment ib)) new)
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
            (set:nremovef (entrances dest) ib)
            (set:nadjoinf (entrances dest) new))
          ;; or predecessors
          (dolist (n (next (end new)))
            (set:nremovef (predecessors n) ib)
            (set:nadjoinf (predecessors n) new))))
    (values ib new)))

(defun compute-iblock-flow-order (function)
  "Compute the forward flow order for the iblocks of FUNCTION, and clean up any existing unreachable iblocks."
  (let ((last nil)
        (existing '()))
    ;; FIXME: Is there a consless way of doing this?
    (do-iblocks (iblock function)
      (push iblock existing))
    (labels ((traverse (iblock)
               (unless (reachedp iblock)
                 (setf (reachedp iblock) t)
                 (dolist (successor (successors iblock))
                   (traverse successor))
                 (if last
                     (setf (%prev last) iblock)
                     (setf (tail function) iblock))
                 (setf (%next iblock) last)
                 (setf last iblock))))
      (traverse (start function)))
    (dolist (iblock existing)
      (if (reachedp iblock)
          (setf (reachedp iblock) nil)
          (clean-up-iblock iblock)))
    (do-iblocks (iblock function)
      (setf (reachedp iblock) nil))
    (values)))
