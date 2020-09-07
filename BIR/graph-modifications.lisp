(in-package #:cleavir-bir)

;;; Remove backpointers to an instruction, etc.
(defgeneric clean-up-instruction (instruction)
  (:method ((instruction instruction))
    (dolist (in (inputs instruction))
      (slot-makunbound in '%use))))

(defmethod clean-up-instruction ((inst readvar))
  (cleavir-set:nremovef (readers (variable inst)) inst))
(defmethod clean-up-instruction ((inst writevar))
  (cleavir-set:nremovef (writers (variable inst)) inst))

;;; Delete an instruction. Must not be a terminator.
(defun delete-instruction ((instruction instruction))
  (check-type instruction (not terminator))
  (clean-up-instruction instruction)
  ;; Delete from inputs.
  ;; Delete from the control flow.
  (let ((pred (predecessor instruction))
        (succ (successor instruction)))
    (assert (not (null succ)))
    (cond ((null pred)
           ;; We start a block.
           ;; There's a KLUDGE here because we don't have a reference to the
           ;; block, and so can't change where its start points to.
           ;; So instead of deleting,
           (change-class instruction 'nop :inputs nil))
          (t
           (setf (predecessor succ) pred
                 (successor pred) succ))))
  (values))

;;; Internal. Replace one value with another in an input list.
(defun replace-input (new old instruction)
  (check-type new linear-datum)
  (check-type old linear-datum)
  (check-type instruction instruction)
  (setf (inputs instruction)
        (nsubstitute new old (inputs instruction) :test #'eq)))

;;; Delete a computation, replacing its use with the given LINEAR-DATUM.
;;; Computation's user is invalid afterward.
(defun delete-computation (computation replacement)
  (check-type computation computation)
  (check-type replacement linear-datum)
  (assert (not (eq computation replacement)))
  (assert (not (slot-boundp replacement '%use)))
  (setf (%use replacement) (%use computation))
  (replace-input replacement computation (%use computation))
  (delete-instruction computation)
  (values))

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
  (flet ((zero (linear-datum)
           (check-type linear-datum linear-datum)
           (slot-makunbound linear-datum '%use)))
    (map-instructions
     (lambda (i)
       (mapc #'zero (inputs i))
       (when (typep i 'linear-datum) (zero i)))
     function))
  ;;; Now add em back
  (map-instructions
   (lambda (i)
     (loop for in in (inputs i)
           do (assert (not (slot-boundp in '%use)))
              (setf (%use in) i)))
   function)
  (values))

(defun refresh-users (top)
  (cleavir-set:mapset nil #'refresh-local-users (all-functions top)))
