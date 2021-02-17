(in-package #:cleavir-loops)

;;; Loops.
;;; A natural loop is the set of nodes associated with a back-edge.
(defclass natural-loop ()
  ((%nodes :accessor nodes :initarg :nodes)
   (%header-node :accessor header-node :initarg :header-node)
   (%back-edge-node :accessor back-edge-node :initarg :back-edge-node)))

;;; We identify back edges and find the natural loop associated with
;;; that back edge.

;;; Return a list of natural loops.
(defun compute-loops (graph)
  (let ((dominance-tree (cleavir-dominance:dominance-tree graph))
        (table (make-hash-table :test #'eq))
        loops)
    (cleavir-graph:with-graph (graph)
      (labels ((compute-natural-loop (header back-edge-node)
                 (let (loop)
                   (labels ((traverse (node)
                              (when (member header (cleavir-dominance:dominators dominance-tree node))
                                (unless (member node loop)
                                  (push node loop)
                                  (cleavir-graph:map-predecessors #'traverse node)))))
                     (traverse back-edge-node))
                   (make-instance 'natural-loop
                     :nodes loop
                     :header-node header
                     :back-edge-node back-edge-node)))
               (traverse (node)
                 (setf (gethash node table) t)
                 (cleavir-graph:do-successors (successor node)
                   (cond ((gethash successor table)
                          ;; If one of the successors has already been
                          ;; traversed, check if that successor dominates
                          ;; this node
                          (when (member successor
                                        (cleavir-dominance:dominators dominance-tree node))
                            (push (compute-natural-loop successor node)
                                  loops)))
                         (t (traverse successor))))))
        (traverse start-node)
        loops))))
