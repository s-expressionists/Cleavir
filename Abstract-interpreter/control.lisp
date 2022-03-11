(in-package #:cleavir-abstract-interpreter)

(defun flow-control (domain instruction info)
  (let* ((old (info domain instruction))
         (new (join domain old info)))
    (multiple-value-bind (sub surety) (subinfop domain new old)
      (when (and surety (not sub))
        (setf (info domain instruction) new)
        (mark instruction)))))

;;; By default, mark all successors with the supremum.
(defmethod flow-instruction ((domain forward-control)
                             (instruction bir:instruction))
  (let ((succ (bir:successor instruction))
        (sup (supremum domain)))
    (if (null succ)
        ;; terminator
        (loop for ib in (bir:next instruction)
              for ninst = (bir:start ib)
              do (flow-control domain ninst sup))
        ;; normal instruction
        (flow-control domain succ sup))))

(defmethod flow-instruction ((domain forward-control)
                             (instruction bir:unwind))
  (let* ((dest (bir:destination instruction))
         (ninst (bir:start dest)))
    (flow-control domain ninst (supremum domain))))

(defmethod flow-instruction ((domain forward-control)
                             (instruction bir:local-call))
  (flow-control domain (bir:start (bir:start (bir:callee instruction)))
                (supremum domain)))

(defmethod flow-instruction ((domain forward-control)
                             (instruction bir:thei))
  (let ((tcf (bir:type-check-function instruction)))
    (unless (symbolp tcf)
      (flow-control domain (bir:start (bir:start tcf)) (supremum domain)))))
