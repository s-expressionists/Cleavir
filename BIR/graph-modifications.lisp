(in-package #:cleavir-bir)

(defgeneric remove-use (datum use))
(defmethod remove-use ((datum linear-datum) use)
  (declare (ignore use))
  (slot-makunbound datum '%use))
(defmethod remove-use ((datum variable) use)
  (cleavir-set:nremovef (cleavir-bir:readers datum) use))

(defgeneric add-use (datum use))
(defmethod add-use ((datum linear-datum) use)
  (assert (not (slot-boundp datum '%use)))
  (setf (%use datum) use))
(defmethod add-use ((datum variable) use)
  (cleavir-set:nadjoinf (cleavir-bir:readers datum) use))

;;; Remove backpointers to an instruction, etc.
(defgeneric clean-up-instruction (instruction)
  (:method-combination progn)
  (:method progn ((instruction instruction))
    (dolist (in (inputs instruction))
      (remove-use in instruction))))

(defgeneric clean-up-iblock (iblock)
  (:method-combination progn)
  (:method progn ((ib iblock))
    (cleavir-set:nremovef (iblocks (function ib)) ib)
    ;; NOTE: clean-up on the terminator disconnects predecessors
    (map-iblock-instructions #'clean-up-instruction (start ib))))

(defgeneric remove-binding (variable binder)
  (:method (variable binder) (declare (ignore variable binder))))
(defmethod remove-binding (variable (binder leti))
  (cleavir-set:nremovef (bindings binder) variable))

;;; If a variable is no longer referenced by a function, remove it from the
;;; function's variable set. If it's no longer referenced at all, remove it from
;;; its function, encloses, and binder if possible.
(defun maybe-clear-variable (variable function)
  (let ((readers (readers variable)) (writers (writers variable))
        (encloses (encloses variable)))
    (cond ((and (zerop (cleavir-set:size readers)) (zerop (cleavir-set:size writers)))
           ;; remove from encloses and functions
           (cleavir-set:doset (e encloses)
             (cleavir-set:nremovef (variables (code e)) variable)
             (cleavir-set:nremovef (variables e) variable))
           ;; and owner, in case owner happens to not be enclosed
           (cleavir-set:nremovef (variables (owner variable)) variable)
           ;; and maybe binder
           (remove-binding variable (binder variable))
           t)
          ((and (cleavir-set:every (lambda (r) (eq (function r) function)) readers)
                (cleavir-set:every (lambda (w) (eq (function w) function)) writers)
                (cleavir-set:every (lambda (e) (eq (function e) function)) encloses))
           (cleavir-set:nremovef (variables function) variable)
           t)
          (t nil))))

(defmethod clean-up-instruction progn ((inst readvar))
  (let ((variable (first (inputs inst))))
    (cleavir-set:nremovef (readers variable) inst)
    (maybe-clear-variable variable (function inst))))
(defmethod clean-up-instruction progn ((inst writevar))
  (let ((variable (first (outputs inst))))
    (cleavir-set:nremovef (writers variable) inst)
    (maybe-clear-variable variable (function inst))))
(defmethod clean-up-instruction progn ((inst enclose))
  (cleavir-set:doset (v (variables inst))
    (cleavir-set:nremovef (encloses v) inst))
  (cleavir-set:nremovef (encloses (code inst)) inst))
(defmethod clean-up-instruction progn ((inst unwind))
  (cleavir-set:nremovef (entrances (destination inst)) (iblock inst)))
(defmethod clean-up-instruction progn ((inst terminator))
  (let ((ib (iblock inst)))
    (dolist (n (next inst)) (cleavir-set:nremovef (predecessors n) ib))))

;;; Delete an instruction. Must not be a terminator.
(defun delete-instruction (instruction)
  (check-type instruction (not terminator))
  (assert (or (typep instruction '(not computation))
              (not (slot-boundp instruction '%use))))
  (clean-up-instruction instruction)
  ;; Delete from inputs.
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
  (check-type old terminator)
  (check-type new terminator)
  (let ((ib (iblock old))
        (new-next (next new)))
    (clean-up-instruction old)
    (let ((pred (predecessor old)))
      (setf (successor pred) new (predecessor new) pred))
    (setf (end ib) new)
    (dolist (n new-next) (cleavir-set:nadjoinf (predecessors n) ib)))
  (values))

(defmethod replace-terminator :after ((new unwind) old)
  (cleavir-set:nadjoinf (entrances (destination new)) (iblock new)))

(defun delete-iblock (iblock)
  ;; FIXME: Should note reasons to the user if nontrivial code is being
  ;; deleted. Or perhaps that should be handled at a higher level?
  (assert (cleavir-set:empty-set-p (predecessors iblock)))
  (clean-up-iblock iblock)
  (let ((successors (successors iblock)))
    (dolist (s successors)
      (cleavir-set:nremovef (predecessors s) iblock)
      (when (cleavir-set:empty-set-p (predecessors s))
        (delete-iblock s)))))

;;; Internal. Replace one value with another in an input list.
(defun replace-input (new old instruction)
  (check-type new linear-datum)
  (check-type old linear-datum)
  (check-type instruction instruction)
  (setf (inputs instruction)
        (nsubstitute new old (inputs instruction) :test #'eq)))

;;; Mark all the inputs of an instruction as being used by that instruction.
;;; This is useful when an instruction is deleted but its inputs maintained.
(defun move-inputs (inst)
  (check-type inst instruction)
  (dolist (input (inputs inst))
    (assert (not (slot-boundp input '%use)))
    (setf (%use input) inst))
  (values))

(defun replace-linear-datum (new old)
  (check-type new linear-datum)
  (check-type old linear-datum)
  (assert (not (slot-boundp new '%use)))
  (setf (%use new) (%use old))
  (replace-input new old (%use old))
  (slot-makunbound old '%use)
  (values))

;;; Delete a computation, replacing its use with the given LINEAR-DATUM.
(defun replace-computation (computation replacement)
  (replace-linear-datum replacement computation)
  (delete-instruction computation)
  (values))

;;; Delete a computation with unused result.
(defun delete-computation (computation)
  (check-type computation computation)
  (assert (not (slot-boundp computation '%use)))
  (delete-instruction computation)
  (values))

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
    ;; Set the new start to lose its predecessor
    (setf (predecessor new-start) nil)
    ;; Move the later instructions
    (setf (start new) new-start (end new) (end ib))
    (loop for i = new-start then (successor i)
          until (null i)
          do (setf (iblock i) new))
    ;; Put a new terminator in the before block
    (let ((new (make-instance 'jump
                 :iblock ib :inputs () :predecessor inst :unwindp nil
                 :next (list new))))
      (setf (successor inst) new (end ib) new))
    ;; Update the new block's presence in predecessors
    (dolist (n (next (end new)))
      (cleavir-set:nremovef (predecessors n) ib)
      (cleavir-set:nadjoinf (predecessors n) new))
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
  (setf (%iblocks function) (reachable-iblocks function)))

(defun refresh-iblocks (top)
  (refresh-local-iblocks top)
  (map-local-instructions
   (lambda (i)
     (typecase i (enclose (refresh-iblocks (code i)))))
   top))

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
  (cleavir-set:mapset nil #'refresh-local-users (all-functions top)))
