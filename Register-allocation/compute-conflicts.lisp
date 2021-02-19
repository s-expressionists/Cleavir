(in-package #:cleavir-register-allocation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute a set of conflicts for the register allocator.  Recall
;;; that two variables generate a conflict when one is live at the
;;; point where the other is written to.  Furthermore, all items that
;;; are written to by the same instruction conflict with each other.
;;;
;;; We do not want multiple copies of some conflict.  We have to be
;;; careful because the relation is symmetric, so that if (L1 . L2) is
;;; a conflict in the set, we do not want to add (L2 . L1) because it
;;; is the same conflict.
;;;
;;; Conflicts are computed only for lexical variables.  Other types of
;;; locations are ignored.

(defun same-conflict-p (c1 c2)
  (or (and (eq (car c1) (car c2))
	   (eq (cdr c1) (cdr c2)))
      (and (eq (car c1) (cdr c2))
	   (eq (cdr c1) (car c2)))))

(defgeneric conflicts-instruction (instruction graph liveness))

(defmethod conflicts-instruction (instruction graph liveness)
  (cleavir-graph:with-graph (graph)
    (let ((conflicts nil)
          (live-after (cleavir-liveness:live-after instruction liveness)))
      (cleavir-graph:do-outputs (output instruction)
        ;; Note that the live-after will include the outputs themselves,
        ;; so we don't need to handle output vs output conflicts specially.
        (cleavir-set:doset (live live-after)
          (unless (eq output live) (push (cons output live) conflicts))))
      conflicts)))

(defun compute-conflicts (graph)
  (let ((conflicts '())
	(table (make-hash-table :test #'eq))
	(liveness (cleavir-liveness:liveness graph)))
    (cleavir-graph:with-graph (graph)
      (cleavir-graph:do-nodes (node)
        (setf conflicts
              (union conflicts (conflicts-instruction graph node liveness)
                     :test #'same-conflict-p))))
    conflicts))
