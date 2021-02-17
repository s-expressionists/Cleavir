(cl:in-package #:cleavir-def-use-chains)

;;; We return a list of def-use chains.  Each def-use chain is
;;; represented by a list whose CAR is the DEFINITION (i.e, a CONS of
;;; a NODE and a VARIABLE), and whose CDR is a list of nodes where the
;;; definition is used.
(defun def-use-chains (graph)
  (let ((reaching-definitions
	  (cleavir-reaching-definitions:reaching-definitions graph))
	(def-use-chains (make-hash-table :test #'eq)))
    (cleavir-graph:with-graph (graph)
      (cleavir-graph:do-nodes (node)
        (loop for reaching in (cleavir-reaching-definitions:reaching
                               reaching-definitions node)
	      do (when (cleavir-graph:input-present-p node (cdr reaching))
		   (push node (gethash reaching def-use-chains))))))
    (let ((result '()))
      (maphash (lambda (definition nodes)
		 (push (cons definition nodes) result))
	       def-use-chains)
      result)))
