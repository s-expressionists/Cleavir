(cl:in-package #:cleavir-def-use-chains-test)

(defun nodes-used-by-definition (graph node var)
  (let ((result '()))
    (cleavir-graph:with-graph (graph)
      (labels ((inputp (node var)
                 (cleavir-graph:do-inputs (in node nil)
                   (when (eq in var) (return t))))
               (outputp (node var)
                 (cleavir-graph:do-outputs (out node nil)
                   (when (eq out var) (return t))))
               (traverse (node path)
	         (unless (or (member node path :test #'eq)
		             (member node result :test #'eq))
                   (when (inputp node var) (push node result))
                   (unless (outputp node var)
                     (cleavir-graph:do-successors (succ node)
                       (traverse succ (cons node path)))))))
        (cleavir-graph:do-successors (succ node)
          (traverse succ '()))))
    result))

(defun test-def-use-chains-one-graph (graph)
  (let ((def-use-chains (cleavir-def-use-chains:def-use-chains graph)))
    ;; First check that each def-use chain is correct, i.e., that the
    ;; use nodes mentioned in it are precisely the nodes that can be
    ;; reached by the definition. 
    (loop for (def . uses) in def-use-chains
	  do (let* ((nodes (nodes-used-by-definition
                            graph (car def) (cdr def))))
	       (assert (and (subsetp nodes uses :test #'eq)
			    (subsetp uses nodes :test #'eq)))))
    ;; Next check that for every definition, there is a def-use-chain
    ;; that containes precisely the nodes that can be reached by that
    ;; definition.
    (cleavir-graph:with-graph (graph)
      (cleavir-graph:do-nodes (node)
        (cleavir-graph:do-outputs (var node)
	  (let ((nodes (nodes-used-by-definition graph node var))
	        (uses (assoc (cons node var) def-use-chains
			     :test #'equal)))
            (assert (and (subsetp nodes uses :test #'eq)
		         (subsetp uses nodes :test #'eq)))))))))

(defun test-def-use-chains (&optional (n 10000))
  (loop repeat n
	do (test-def-use-chains-one-graph
            (cleavir-graph-test-utilities:random-flow-chart))))
