(in-package #:cleavir-abstract-interpreter)

(defclass reachability (forward-control) ())

(defun reachability (product)
  (product-domain-of-type 'reachability product))

;;; Reachability info is T (maybe reachable) or NIL (not reachable).

(defmethod infimum ((domain reachability)) nil)
(defmethod supremum ((domain reachability)) t)
(defmethod subinfop ((domain reachability) info1 info2) (or info2 (not info1)))
(defmethod join/2 ((domain reachability) info1 info2) (or info1 info2))
(defmethod wjoin/2 ((domain reachability) info1 info2) (or info1 info2))
(defmethod meet/2 ((domain reachability) info1 info2) (and info1 info2))

;;; By default, pass reachability through.
(defmethod interpret-instruction ((strategy strategy)
                                  (domain reachability) (product product)
                                  (instruction bir:instruction))
  (let ((succ (bir:successor instruction))
        (info (info strategy domain instruction)))
    (if (null succ)
        ;; terminator
        (loop for ib in (bir:next instruction)
              for ninst = (bir:start ib)
              do (flow-control strategy domain ninst info))
        ;; normal instruction
        (flow-control strategy domain succ info))))

(defmethod interpret-instruction ((strategy strategy) (domain reachability)
                                  (product product) (instruction bir:unwind))
  (let* ((dest (bir:destination instruction))
         (ninst (bir:start dest)))
    (flow-control strategy domain ninst (info strategy domain instruction))))

(defmethod interpret-instruction ((strategy strategy) (domain reachability)
                                  (product product)
                                  (instruction bir:local-call))
  (flow-control strategy domain
                (bir:start (bir:start (bir:callee instruction)))
                (info strategy domain instruction)))

(defmethod interpret-instruction ((strategy strategy) (domain reachability)
                                  (product product) (instruction bir:thei))
  (let ((tcf (bir:type-check-function instruction)))
    (unless (symbolp tcf)
      (flow-control strategy domain (bir:start (bir:start tcf))
                    (info strategy domain instruction)))))
