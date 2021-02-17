(in-package #:cleavir-flow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TRAVERSAL.
;;;
;;; A traversal represents a way of carrying out a dataflow analysis.
;;; Traversals are independent from flow classes, i.e. any flow class can be
;;; used with any traversal class. The traversal only defines a way of
;;; traversing a graph to carry out an analysis.
;;;
;;; Custom traversals must inherit from TRAVERSAL, and define methods on the
;;; MARK and WORK functions, and optionally the INITIALIZE function.
;;;

(defclass traversal () ; abstract
  ((%flow :accessor %flow)
   (%direction :initarg :direction :initform nil :reader direction)))

;;; Default implementation: Serially processed stack.

(defclass worklist (traversal)
  ((%list :initform nil :accessor worklist-list)))

(defmethod mark ((traversal worklist) node)
  (pushnew node (worklist-list traversal) :test #'eq))

(defmethod work ((traversal worklist) graph)
  (loop for work = (pop (worklist-list traversal))
        do (flow (%flow traversal) graph work)
        until (null (worklist-list traversal))))

(defmethod initialize ((traversal traversal) graph)
  (cleavir-graph:with-graph (graph)
    (if (eq (direction traversal) :forward)
        (mark traversal (cleavir-graph:root))
        (cleavir-graph:do-nodes (node) (mark traversal node)))))
