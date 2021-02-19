(cl:in-package #:cleavir-dominance-test)

(defun name (node)
  (cleavir-graph-test-utilities:name node))

;;; This algorithm applies the definition of dominators by examining
;;; all possible paths from the root of the flow chart to a node N,
;;; and removing any node not on that path from the possible
;;; dominators of N.
(defun slow-dominators (graph)
  (cleavir-graph:with-graph (graph)
    (let* ((preorder (cleavir-graph:depth-first-preorder))
	   (dominators (let ((table (make-hash-table :test #'eq)))
		         (loop for node in preorder
			       do (setf (gethash node table) preorder))
		         table))
           (root (cleavir-graph:root)))
      (flet ((dominators (node)
	       (gethash node dominators))
	     ((setf dominators) (new-dominators node)
	       (setf (gethash node dominators) new-dominators)))
        (dolist (node preorder)
          (labels ((traverse (n path)
		     (cond ((eq n node)
			    (setf (dominators node)
				  (intersection (dominators node)
						(cons n path)
						:test #'eq)))
			   ((not (member n path :test #'eq))
                            (cleavir-graph:do-successors (succ n)
                              (traverse succ (cons n path)))))))
	    (traverse root '()))))
      dominators)))

(defun test-one-chart (graph)
  (cleavir-graph:with-graph (graph)
    (let ((d1 (cleavir-dominance:dominance-tree graph))
	  (d2 (slow-dominators graph)))
      (cleavir-graph:do-nodes-depth-first-preorder (node)
        (unless (and (null (set-difference (gethash node d1)
					   (gethash node d2)))
		     (null (set-difference (gethash node d2)
					   (gethash node d1))))
	  (return (values node d1 d2)))))))

(defun test-dominance (&optional (n 10000))
  (loop repeat n
	do (let ((f (cleavir-graph-test-utilities:random-flow-chart)))
             (cleavir-graph:with-graph (f)
	       (when (< (cleavir-graph:size) 50)
	         (test-one-chart f))))))

(defun draw-dominance-tree (dominance-tree filename)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede)
    (format stream "digraph G {~%   ordering = in;~%")
    ;; Draw all the nodes first.
    (loop for node being each hash-key of dominance-tree
	  do (format stream "   ~a [label = \"~a\"];~%"
		     (name node) (name node)))
    ;; Now draw all the arcs.
    (loop for node being each hash-key of dominance-tree
	  do (let ((idom (cleavir-dominance:immediate-dominator
			  dominance-tree node)))
	       (unless (null idom)
		 (format stream "   ~a -> ~a;~%"
			 (name node) (name idom)))))
    (format stream "}~%")))

(defun draw-dominance-frontiers
    (graph dominance-frontiers filename)
  (cleavir-graph:with-graph (graph)
    (let ((table (make-hash-table :test #'eq)))
      (with-open-file (stream filename
			      :direction :output
			      :if-exists :supersede)
	(format stream "digraph G {~% ordering = out;~%")
	(labels ((draw-node (node)
		   (unless (gethash node table)
		     (setf (gethash node table) t)
                     (cleavir-graph:map-successors #'draw-node node)
		     (format stream
			     "   ~a [label = \"~a\"];~%"
			     (name node) (name node))
                     (cleavir-graph:do-successors (succ node)
                       (format stream
			       "   ~a -> ~a;~%"
			       (name node)
			       (name succ))))))
	  (draw-node graph))
	(loop for node being each hash-key of dominance-frontiers
	      do (let ((f (gethash node dominance-frontiers)))
		   (loop for ff in f
			 do (format stream
				    "   ~a -> ~a [style = bold, color = red];"
				    (name node) (name ff)))))
	(format stream "}~%")))))

;;; The dominance frontier of a node X is defined to be the set of all
;;; nodes Y such that 1: there exists a predecessor Z of Y such that X
;;; dominates Z, and 2: X does not strictly dominate Y.
(defun slow-dominance-frontiers (graph)
  (let ((dominance-tree (cleavir-dominance:dominance-tree graph))
	(result (make-hash-table :test #'eq)))
    (flet ((dominates (n1 n2)
	     (member n1 (cleavir-dominance:dominators
			 dominance-tree n2)))
	   (strictly-dominates (n1 n2)
	     (member n1 (cleavir-dominance:strict-dominators
			 dominance-tree n2))))
      (cleavir-graph:with-graph (graph)
        (cleavir-graph:do-nodes-depth-first-preorder (x)
          (cleavir-graph:do-nodes-depth-first-preorder (y)
            (when (and (cleavir-graph:do-predecessors (z y nil)
                         (when (dominates x z) (return t)))
		       (not (strictly-dominates x y)))
	      (push y (gethash x result)))))))
    result))

(defun test-dominance-frontiers-one-flow-chart (graph)
  (flet ((same-set-p (s1 s2)
	   (and (null (set-difference s1 s2 :test #'eq))
		(null (set-difference s2 s1 :test #'eq)))))
    (let ((f1 (cleavir-dominance:dominance-frontiers graph))
	  (f2 (slow-dominance-frontiers graph)))
      (loop for node being each hash-key of f1
	    do (unless (same-set-p (gethash node f1) (gethash node f2))
		 (throw 'discrepancy (values graph f1 f2))))
      (loop for node being each hash-key of f2
	    do (unless (same-set-p (gethash node f1) (gethash node f2))
		 (throw 'discrepancy (values graph f1 f2)))))))

(defun test-dominance-frontiers (&optional (n 10000))
  (catch 'discrepancy
    (loop repeat n
	  do (test-dominance-frontiers-one-flow-chart
	      (cleavir-graph-test-utilities:random-flow-chart)))))

(defun test (&optional (n 10000)) (test-dominance-frontiers n))
