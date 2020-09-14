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

;;; If a variable is no longer referenced by a function, remove it from the
;;; function's variable set.
(defun maybe-clear-variable (variable function)
  (cleavir-set:doset (r (readers variable))
    (when (eq (function r) function) (return-from maybe-clear-variable nil)))
  (cleavir-set:doset (w (writers variable))
    (when (eq (function w) function) (return-from maybe-clear-variable nil)))
  (cleavir-set:doset (e (encloses variable))
    (when (eq (function e) function) (return-from maybe-clear-variable nil)))
  (cleavir-set:nremovef (variables function) variable)
  t)
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
        (old-next (next old))
        (new-next (next new)))
    (clean-up-instruction old)
    (setf (end ib) new)
    (dolist (n old-next) (cleavir-set:nremovef (predecessors n) ib))
    (dolist (n new-next) (cleavir-set:nadjoinf (predecessors n) ib)))
  (values))

(defmethod replace-terminator :after ((new unwind) old)
  (cleavir-set:nadjoinf (entrances (destination new)) (iblock new)))

(defun delete-iblock (iblock)
  (assert (cleavir-set:empty-set-p (predecessors iblock)))
  (let ((successors (successors iblock)))
    (map-iblock-instructions #'clean-up-instruction (start iblock))
    (dolist (s successors)
      (cleavir-set:nremovef (predecessors s) iblock))))

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
         (new (make-instance 'iblock
                :function (function ib) :inputs nil
                :dynamic-environment (dynamic-environment ib)))
         (new-start (successor inst)))
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
