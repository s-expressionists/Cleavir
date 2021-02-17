(in-package #:cleavir-reaching-definitions-test)

;;;; To test the reaching definitions, we essentially program it
;;;; again, in a different way.  This time we take a single
;;;; definition, and we see what nodes it reaches by traversing the
;;;; graph starting at that node.  We call this function for every
;;;; definition in some random graph, and we compare the result to
;;;; what the call to REACHING-DEFINITIONS returns.  We compare it
;;;; both ways, i.e., if some node can be reached the stupid way, then
;;;; its definition had better be in the result of
;;;; REACHING-DEFINITIONS, and if some node is not in the result of
;;;; REACHING-DEFINITIONS, then it must not be possible to reach it
;;;; the stupid way.

;;; Return a hash table in which the keys are the nodes that can be
;;; reached from a definition of VAR in NODE.
(defun nodes-reached-by-definition (graph node var)
  (cleavir-graph:with-graph (graph)
    (let ((table (make-hash-table :test #'eq)))
      (labels ((traverse (node path)
	         (if (member node path :test #'eq)
		     nil
		     (unless (gethash node table)
		       (setf (gethash node table) t)
		       (unless (cleavir-graph:do-outputs (out node nil)
                                 (when (eq var out) (return t)))
                         (let ((new-path (cons node path)))
                           (cleavir-graph:do-successors (succ node)
                             (traverse succ new-path))))))))
        (cleavir-graph:do-successors (succ node) (traverse succ '())))
      table)))

(defun test-reaching-definitions-on-one-graph (graph)
  (let ((reaching-definitions
	  (cleavir-reaching-definitions:reaching-definitions graph)))
    (cleavir-graph:with-graph (graph)
      (cleavir-graph:do-nodes (definer)
        (cleavir-graph:do-outputs (var definer)
          (let ((nodes-reached
                  (nodes-reached-by-definition graph definer var)))
	    (loop for n being each hash-key of nodes-reached
                  for ntable = (cleavir-reaching-definitions:reaching
                                n reaching-definitions)
		  do (assert (eq definer (gethash var ntable))))
            (cleavir-graph:do-nodes (user)
              (let ((utable (cleavir-reaching-definitions:reaching
                             user reaching-definitions)))
                (if (gethash user nodes-reached)
                    (assert (eq definer (gethash var utable)))
                    (assert (not (eq definer (gethash var utable)))))))))))))

(defun test-reaching-definitions (&optional (n 10000))
  (loop repeat n
	do (test-reaching-definitions-on-one-graph
            (cleavir-graph-test-utilities:random-flow-chart))))
