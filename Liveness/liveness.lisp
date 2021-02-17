(in-package #:cleavir-liveness)

(cleavir-flow:define-flow liveness ()
  ((%live-before :accessor %live-before :reader live-before
                 :initform (cleavir-set:empty-set))
   (%live-after :accessor %live-after :reader live-after
                :initform (cleavir-set:empty-set))))

(defmethod cleavir-flow:flow ((flow liveness) graph node)
  (cleavir-graph:with-graph (graph)
    (let ((live-before (%live-before node flow)))
      (unless (cleavir-graph:do-inputs (input node t) ; every
                (unless (cleavir-set:presentp input live-before) (return nil)))
        (cleavir-graph:do-inputs (input node)
          (cleavir-set:nadjoinf (%live-before node flow) input))
        (cleavir-graph:do-predecessors (p node)
          (cleavir-set:nunionf (%live-after p flow) live-before)
          ;; To the predecessor's live-before, add our live-before,
          ;; except for any outputs of the predecessor.
          ;; basically union with subtraction, but we avoid consing.
          (cleavir-set:doset (elem live-before)
            (unless (cleavir-graph:do-outputs (output p nil) ; some
                      (when (eq elem output) (return t)))
              (cleavir-set:nadjoinf (%live-before p flow) elem)))
          ;; Mark that predecessor for processing
          (cleavir-flow:mark flow p))))))
