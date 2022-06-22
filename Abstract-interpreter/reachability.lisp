(in-package #:cleavir-abstract-interpreter)

(defclass reachability (noetherian-mixin forward-control) ())

;;; Reachability info is T (maybe reachable) or NIL (not reachable).

(defmethod infimum ((domain reachability)) nil)
(defmethod supremum ((domain reachability)) t)
(defmethod subinfop ((domain reachability) info1 info2) (or info2 (not info1)))
(defmethod join/2 ((domain reachability) info1 info2) (or info1 info2))
(defmethod meet/2 ((domain reachability) info1 info2) (and info1 info2))

;;; Just pass reachability through.
;;; Refinements to this analysis use additional information; for example see
;;; typed-reachability.lisp.
(defmethod flow-instruction ((domain reachability) (instruction bir:instruction)
                             &rest infos)
  (let ((info (first infos)))
    (if (or (bir:successor instruction)
            (= (length (bir:next instruction)) 1))
        info
        (values-list (make-list (length (bir:next instruction)) :initial-element info)))))
