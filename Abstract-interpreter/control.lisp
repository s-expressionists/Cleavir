(in-package #:cleavir-abstract-interpreter)

(defgeneric flow-control (strategy domain instruction info))

(defmethod flow-control ((strategy optimism) domain instruction info)
  (let* ((old (info strategy domain instruction))
         (new (join domain old info)))
    (multiple-value-bind (sub surety) (subinfop domain new old)
      (when (and surety (not sub))
        (setf (info strategy domain instruction) new)
        (mark strategy instruction)))))

;;; By default, mark all successors with the supremum.
(defmethod interpret-instruction ((strategy strategy)
                                  (domain forward-control) (product product)
                                  (instruction bir:instruction))
  (let ((succ (bir:successor instruction))
        (sup (supremum domain)))
    (if (null succ)
        ;; terminator
        (loop for ib in (bir:next instruction)
              for ninst = (bir:start ib)
              do (flow-control strategy domain ninst sup))
        ;; normal instruction
        (flow-control strategy domain succ sup))))

(defmethod interpret-instruction ((strategy strategy) (domain forward-control)
                                  (product product) (instruction bir:unwind))
  (let* ((dest (bir:destination instruction))
         (ninst (bir:start dest)))
    (flow-control strategy domain ninst (supremum domain))))

(defmethod interpret-instruction ((strategy strategy) (domain forward-control)
                                  (product product) (instruction bir:local-call))
  (flow-control strategy domain
                (bir:start (bir:start (bir:callee instruction)))
                (supremum domain)))

(defmethod interpret-instruction ((strategy strategy) (domain forward-control)
                                  (product product) (instruction bir:thei))
  (let ((tcf (bir:type-check-function instruction)))
    (unless (symbolp tcf)
      (flow-control strategy domain (bir:start (bir:start tcf))
                    (supremum domain)))))

(defmethod flow-call ((strategy strategy) (domain forward-control)
                      (function bir:function) info)
  (flow-control strategy domain (bir:start (bir:start function)) info))
