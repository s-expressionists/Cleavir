(in-package #:cleavir-bir)

(defmethod (setf dynamic-environment) :before
    ((nde dynamic-environment) (obj iblock))
  (when (slot-boundp obj '%dynamic-environment)
    (cleavir-set:nremovef (scope (dynamic-environment obj)) obj))
  (cleavir-set:nadjoinf (scope nde) obj))

;;; Maintaining use and definition sets

(defgeneric remove-use (datum use))
(defmethod remove-use ((datum linear-datum) use)
  (declare (cl:ignore use))
  (setf (%use datum) nil))
(defmethod remove-use ((datum variable) use)
  (cleavir-set:nremovef (cleavir-bir:readers datum) use))
(defmethod remove-use ((datum constant) use)
  (cleavir-set:nremovef (cleavir-bir:readers datum) use))
(defmethod remove-use ((datum load-time-value) use)
  (cleavir-set:nremovef (cleavir-bir:readers datum) use))
(defmethod remove-use ((datum function) (use abstract-local-call))
  (cleavir-set:nremovef (local-calls datum) use))

(defgeneric add-use (datum use))
(defmethod add-use ((datum linear-datum) use)
  (assert (null (use datum))
          ()
          "Tried to add a use ~a to datum ~a, which is already in use by ~a"
          use datum (use datum))
  (setf (%use datum) use))
(defmethod add-use ((datum variable) use)
  (cleavir-set:nadjoinf (readers datum) use))
(defmethod add-use ((datum constant) use)
  (cleavir-set:nadjoinf (readers datum) use))
(defmethod add-use ((datum load-time-value) use)
  (cleavir-set:nadjoinf (readers datum) use))
(defmethod add-use ((datum function) (use abstract-local-call))
  (cleavir-set:nadjoinf (local-calls datum) use))

(defmethod (setf inputs) :before (new-inputs (inst instruction))
  (dolist (input (inputs inst))
    (remove-use input inst))
  (dolist (input new-inputs)
    (add-use input inst)))

(defgeneric remove-definition (datum definition)
  (:method ((datum datum) (definition instruction))))
(defmethod remove-definition ((datum output) (definition instruction))
  (setf (%definition datum) nil))

(defgeneric add-definition (datum definition)
  (:method ((datum datum) (definition instruction))))
(defmethod add-definition ((datum output) (definition instruction))
  (assert (null (%definition datum)))
  (setf (%definition datum) definition))
(defmethod add-definition ((datum variable) (definition instruction))
  (cleavir-set:nadjoinf (writers datum) definition))

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
          (predecessor new) existing (successor new) succ
          (iblock new) (iblock existing)))
  (values))

;;; Remove backpointers to an instruction, etc.
(defgeneric clean-up-instruction (instruction)
  (:method-combination progn)
  (:method progn ((instruction instruction))
    (dolist (in (inputs instruction))
      (remove-use in instruction))))

(defgeneric clean-up-iblock (iblock)
  (:method-combination progn)
  (:method progn ((ib iblock))
    (cleavir-set:nremovef (scope (dynamic-environment ib)) ib)
    ;; NOTE: clean-up on the terminator disconnects predecessors
    (when (slot-boundp ib '%start)
      (map-iblock-instructions #'clean-up-instruction ib))))

(defgeneric clean-up-function (function)
  (:method-combination progn)
  (:method progn ((function function))
    (cleavir-set:nremovef (functions (module function)) function)
    (map-iblocks #'clean-up-iblock function)))

;;; Remove a variable from a module.
(defun clean-up-variable (variable)
  (cleavir-set:nremovef (variables (function variable)) variable)
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
    (cleavir-set:nremovef (readers variable) inst)
    ;; If a variable is no longer referenced, remove all of its
    ;; writers.
    (when (cleavir-set:empty-set-p (cleavir-bir:readers variable))
      (cleavir-set:doset (writer (writers variable))
        (delete-instruction writer)))))
(defmethod clean-up-instruction progn ((inst writevar))
  (let ((variable (output inst)))
    (cleavir-set:nremovef (writers variable) inst)
    ;; When the variable no longer has any writers or readers, clean it
    ;; up.
    (when (and (cleavir-set:empty-set-p (cleavir-bir:writers variable))
               (cleavir-set:empty-set-p (cleavir-bir:readers variable)))
      (clean-up-variable variable))))
(defmethod clean-up-instruction progn ((inst constant-reference))
  (let ((constant (first (inputs inst))))
    (cleavir-set:nremovef (readers constant) inst)
    (when (cleavir-set:empty-set-p (readers constant))
      (cleavir-set:nremovef (constants (module (function inst))) constant))))
(defmethod clean-up-instruction progn ((inst load-time-value-reference))
  (let ((ltv (first (inputs inst))))
    (cleavir-set:nremovef (readers ltv) inst)
    (when (cleavir-set:empty-set-p (readers ltv))
      (cleavir-set:nremovef (load-time-values (module (function inst))) ltv))))
(defmethod clean-up-instruction progn ((inst enclose))
  (let ((code (code inst)))
    (setf (enclose code) nil)
    (when (cleavir-set:empty-set-p (local-calls code))
      (clean-up-function code))))
(defmethod clean-up-instruction progn ((inst abstract-local-call))
  (let* ((code (callee inst))
         (local-calls (local-calls code)))
    (cleavir-set:nremovef local-calls inst)
    (when (and (null (enclose code))
               (cleavir-set:empty-set-p local-calls))
      (clean-up-function code))))
(defmethod clean-up-instruction progn ((inst unwind))
  (cleavir-set:nremovef (entrances (destination inst)) (iblock inst))
  (cleavir-set:nremovef (unwinds (catch inst)) inst))
(defmethod clean-up-instruction progn ((inst catch))
  (cleavir-set:nremovef (catches (function inst)) inst))
(defmethod clean-up-instruction progn ((inst terminator))
  (let ((ib (iblock inst)))
    (dolist (n (next inst)) (cleavir-set:nremovef (predecessors n) ib))))
(defmethod clean-up-instruction progn ((inst returni))
  (setf (returni (function inst)) nil))
(defmethod clean-up-instruction progn ((inst thei))
  (let ((type-check-function (type-check-function inst)))
    (unless (symbolp type-check-function)
      (assert (and (null (enclose type-check-function))
                   (cleavir-set:empty-set-p (local-calls type-check-function)))
              ()
              "Type check function for THEI should not have local calls or enclose!")
      (clean-up-function type-check-function))))

;;; Remove a THEI by forwarding its input to its use.
(defun delete-thei (thei)
  (let ((input (first (inputs thei))))
    (setf (inputs thei) '())
    (replace-uses input (output thei))
    (delete-instruction thei)))

;;; Return a copy of a list containing all but the indexed element.
(defun list-sans-index (list index)
  (loop for elem in list for p from 0 unless (= index p) collect elem))

(defun delete-phi (phi)
  (let ((iblock (iblock phi)))
    (setf (cleavir-bir:inputs iblock)
          (delete phi (cleavir-bir:inputs iblock)))
    (cleavir-set:doset (def (definitions phi))
      (let ((pos (position phi (cleavir-bir:outputs def))))
        (setf (cleavir-bir:outputs def)
              (list-sans-index (cleavir-bir:outputs def) pos))
        (setf (cleavir-bir:inputs def)
              (list-sans-index (cleavir-bir:inputs def) pos))))))

;;; Delete an instruction. Must not be a terminator.
(defun delete-instruction (instruction)
  (check-type instruction (and instruction (not terminator)))
  (assert (every #'unused-p (outputs instruction)))
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
  (declare (cl:ignore old))
  (cleavir-set:nadjoinf (entrances (destination new)) (iblock new)))

(defmethod replace-terminator :after (new (old unwind))
  (declare (cl:ignore new))
  (cleavir-set:nremovef (entrances (destination old)) (iblock old)))

(defun orphan-iblock-p (iblock)
  (and (cleavir-set:empty-set-p (predecessors iblock))
       (cleavir-set:empty-set-p (entrances iblock))))

(defun delete-iblock (iblock)
  ;; FIXME: Should note reasons to the user if nontrivial code is being
  ;; deleted. Or perhaps that should be handled at a higher level?
  (assert (orphan-iblock-p iblock))
  (clean-up-iblock iblock)
  (when (slot-boundp iblock '%end)
    (let ((successors (successors iblock)))
      (dolist (s successors)
        (cleavir-set:nremovef (predecessors s) iblock)
        (when (orphan-iblock-p s)
          (delete-iblock s)))))
  (remove-iblock-from-flow-order iblock))

(defun maybe-delete-iblock (iblock)
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
  (assert (null (use new)))
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
    (setf (%next prev) next)
    (if next
        (setf (%prev next) prev)
        (setf (tail (function iblock)) prev))))

;;; Insert the new iblock into the flow order after AFTER.
(defun insert-iblock-into-flow-order (new after)
  (let ((next (%next after)))
    (setf (%next after) new)
    (setf (%prev new) after)
    (setf (%next new) next)
    (if next
        (setf (%prev next) new)
        (setf (tail (function after)) new))))

;;; Merge IBLOCK to its unique successor if possible, returning false
;;; if not.
(defun merge-successor-if-possible (iblock)
  (when (successor-mergable-p iblock)
    (merge-successor iblock)))

(defun successor-mergable-p (iblock)
  (let ((successors (successors iblock)))
    (and successors
         (typep (end iblock) 'cleavir-bir:jump)
         (let* ((successor (first successors))
                (predecessors (predecessors successor)))
           (and (= (cleavir-set:size predecessors) 1)
                (cleavir-set:empty-set-p (entrances successor))
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
      (setf (cleavir-bir:iblock instruction) iblock))
    ;; Propagate the inputs of the jump into the uses of the second
    ;; block's phis.
    (mapc (lambda (input phi)
            (remove-use input jump)
            (replace-uses input phi))
          (inputs jump) (inputs successor))
    (if (typep end 'unwind)
        ;; Update the new block's presence in entrances
        (let ((dest (destination end)))
          (cleavir-set:nremovef (entrances dest) successor)
          (cleavir-set:nadjoinf (entrances dest) iblock))
        ;; Update the predecessors of the successors.
        (dolist (succ (successors successor))
          (cleavir-set:nremovef (predecessors succ) successor)
          (cleavir-set:nadjoinf (predecessors succ) iblock)))
    (remove-iblock-from-flow-order successor)
    ;; Delete from scope.
    (cleavir-set:nremovef (scope (dynamic-environment successor)) successor)
    ;; The successor block is now conceptually deleted.
    (setf (function successor) nil)
    iblock))

(defun empty-iblock-p (iblock)
  (let ((start (cleavir-bir:start iblock)))
    (and (typep start 'cleavir-bir:jump)
         (not (cleavir-bir:unwindp start))
         (null (cleavir-bir:inputs iblock))
         (null (cleavir-bir:outputs start))
         (not (eq (start (function iblock)) iblock))
         (not (eq iblock (first (next start))))
         (cleavir-set:empty-set-p (entrances iblock)))))

;;; Forward the predecessors of iblock to the successor of iblock if
;;; it is empty.
(defun delete-iblock-if-empty (iblock)
  (when (empty-iblock-p iblock)
    (let ((successor (first (successors iblock)))
          (predecessors (predecessors iblock)))
      (cleavir-set:doset (predecessor predecessors)
        (let ((end (end predecessor)))
          (nsubstitute successor iblock (next end)))
        (cleavir-set:nadjoinf (predecessors successor) predecessor))
      (cleavir-set:nremovef (predecessors successor) iblock))
    (remove-iblock-from-flow-order iblock)
    ;; Remove from scope.
    (cleavir-set:nremovef (scope (dynamic-environment iblock)) iblock)
    iblock))

;;; Split a iblock into two iblocks.
(defun split-block-after (inst)
  (check-type inst (and instruction (not terminator)))
  ;; the new block is the block after, because there's a little less to update.
  (let* ((ib (iblock inst))
         (new (make-instance 'iblock
                :function (function ib) :inputs nil
                :predecessors (cleavir-set:make-set ib)
                :dynamic-environment (dynamic-environment ib)))
         (new-start (successor inst)))
    (insert-iblock-into-flow-order new ib)
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
    (values ib new)))

;;; Compute the forward flow order for the iblocks of FUNCTION and
;;; clean up any existing unreachable iblocks.
(defun compute-iblock-flow-order (function)
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
