(defpackage #:cleavir-graph
  (:use #:cl)
  (:export #:with-graph #:graph-functions)
  (:export #:root)
  (:export #:map-nodes #:map-nodes-depth-first-preorder)
  (:export #:do-nodes #:do-nodes-depth-first-preorder
           #:depth-first-preorder)
  (:export #:map-inputs #:map-outputs #:map-predecessors #:map-successors)
  (:export #:do-inputs #:do-outputs #:do-predecessors #:do-successors))
