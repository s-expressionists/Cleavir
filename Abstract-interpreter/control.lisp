(in-package #:cleavir-abstract-interpreter)

(defgeneric flow-control (strategy domain instruction info))

(defmethod flow-control ((strategy optimism) domain instruction info)
  (let* ((old (info strategy domain instruction))
         (new (join domain old info)))
    (multiple-value-bind (sub surety) (subinfop domain new old)
      (when (and surety (not sub))
        (setf (info strategy domain instruction) new)
        (mark strategy instruction)))))

;;; Compute and return the info from INSTRUCTION as it goes to NEXT.
;;; This is useful for pessimistic analysis, as seen below.
(defgeneric compute-control (domain instruction next))

;;; By default, mark all successors with the supremum.
(defmethod compute-control ((domain domain) (instruction bir:instruction)
                            (next bir:instruction))
  (supremum domain))

(defmethod flow-control ((strategy pessimism) domain instruction info)
  (let* ((ib (bir:iblock instruction))
         (old (info strategy domain instruction))
         (info
           (if (and (eq instruction (bir:start ib))
                    ;; See KLUDGE in flow-call, below.
                    (not (eq ib (bir:start (bir:function ib)))))
               ;; We're at the head of a block, so we have to merge (join) infos
               ;; from all predecessor blocks. This means we actually ignore
               ;; the passed-in info. This is analogous to the pessimistic
               ;; FLOW-DATUM in data.lisp.
               (let ((accum (infimum domain)))
                 (set:doset (pred (bir:predecessors ib))
                   (let* ((end (bir:end pred))
                          (info (compute-control domain end instruction)))
                     (setf accum (wjoin domain accum info))))
                 (set:doset (pred (bir:entrances ib))
                   (let* ((end (bir:end pred))
                          (info (compute-control domain end instruction)))
                     (setf accum (wjoin domain accum info))))
                 accum)
               info))
         (new (meet domain old info)))
    (multiple-value-bind (sub surety) (subinfop domain old new)
      (when (and surety (not sub))
        (setf (info strategy domain instruction) new)
        (mark strategy instruction)))))

(defmethod interpret-instruction ((strategy strategy)
                                  (domain forward-control) (product product)
                                  (instruction bir:instruction))
  (let ((succ (bir:successor instruction)))
    (if (null succ)
        ;; terminator
        (loop for ib in (bir:next instruction)
              for ninst = (bir:start ib)
              for info = (compute-control domain instruction ninst)
              do (flow-control strategy domain ninst info))
        ;; normal instruction
        (flow-control strategy domain succ
                      (compute-control domain instruction succ)))))

(defmethod interpret-instruction ((strategy strategy) (domain forward-control)
                                  (product product) (instruction bir:unwind))
  (let* ((dest (bir:destination instruction))
         (ninst (bir:start dest))
         (info (compute-control domain instruction ninst)))
    (flow-control strategy domain ninst info)))

(defmethod interpret-instruction ((strategy strategy) (domain forward-control)
                                  (product product) (instruction bir:local-call))
  (let ((ninst (bir:start (bir:start (bir:callee instruction)))))
    (flow-control strategy domain ninst
                  (compute-control domain instruction ninst))))

(defmethod interpret-instruction ((strategy strategy) (domain forward-control)
                                  (product product) (instruction bir:thei))
  (let ((tcf (bir:type-check-function instruction)))
    (unless (symbolp tcf)
      (flow-control strategy domain (bir:start (bir:start tcf))
                    (supremum domain)))))

(defmethod flow-call ((strategy strategy) (domain forward-control)
                      (function bir:function) info)
  (flow-control strategy domain (bir:start (bir:start function)) info))

(defmethod flow-call ((strategy pessimism) (domain forward-control)
                      (function bir:function) info)
  (declare (ignore info))
  ;; KLUDGE: I'm not sure how to merge/join information from multiple calls
  ;; with a pessimistic strategy, so for now we just always pass in the sup.
  (flow-control strategy domain (bir:start (bir:start function))
                (supremum domain)))
