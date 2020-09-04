(in-package #:cleavir-bir)

(defun delete-instruction (instruction)
  (check-type instruction (and instruction (not terminator)))
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
  (check-type new value)
  (check-type old value)
  (check-type instruction instruction)
  (setf (inputs instruction)
        (nsubstitute new old (inputs instruction) :test #'eq)))

;;; Replace a value with another value, keeping USERS sets updated.
(defun replace-value (value replacement)
  (check-type value value)
  (check-type replacement value)
  (assert (not (eq value replacement)))
  (mapset (lambda (user)
            (replace-input replacement value user)
            (nset-adjoinf (%users replacement) user))
          (%users value))
  (setf (%users value) (nset-empty (%users value)))
  (values))

;;; Delete a computation, replacing all of its uses with the given VALUE.
;;; Maintains the sets of users (except for the deleted computation).
(defun delete-computation (computation replacement)
  (check-type computation computation)
  (check-type replacement value)
  (assert (not (eq computation replacement)))
  (mapset (lambda (user)
            (setf (inputs user)
                  (nsubstitute replacement computation (inputs user)
                               :test #'eq))
            (nset-adjoinf (%users replacement) user))
          (users computation))
  (delete-instruction computation)
  (values))

(defun reachable-iblocks (function)
  (check-type function function)
  (let ((set (empty-set))
        (worklist (list (start function))))
    (loop for work = (pop worklist)
          until (null work)
          unless (presentp work set)
            do (nset-adjoinf set work)
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
  ;;; First zero out existing user sets
  (flet ((zero (value)
           (unless (empty-set-p (%users value))
             (setf (%users value) (empty-set)))))
    (map-instructions
     (lambda (i)
       (mapc #'zero (inputs i))
       (when (typep i 'value) (zero i)))
     function))
  ;;; Now add em back
  (map-instructions
   (lambda (i)
     (loop for in in (inputs i)
           do (setf (%users in) (nset-adjoin i (%users in)))))
   function)
  (values))

(defun refresh-users (top)
  (mapset #'refresh-local-users (all-functions top)))
