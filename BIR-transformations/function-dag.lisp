(in-package #:cleavir-bir-transformations)

#|
The directed acyclic graph (DAG) of functions describes the closures in a
program. Every function except the topmost function is the CODE of one or more
ENCLOSE instructions, and each enclose instruction is owned by another function.
For example in
(lambda (x) (lambda () (lambda () x)))
The outer function is topmost, the middle function has one enclose owned by the
outer function, and the inner function has one enclose owned by the middle
function.
Cycles are not possible. In fact in a normal Lisp program, the DAG is more
specifically a tree, because there is at most one enclose per function; but
some transformations, especially inlining, can change this.
|#

(defclass dag-node ()
  (;; A set of DAG-NODEs; the NODE-FUNCTION of this node owns an enclose
   ;; of every child.
   (%children :initform (cleavir-set:empty-set)
              :reader children :accessor %children)))

(defgeneric node-function (node))

(defclass function-dag (dag-node)
  (;; The highest level function
   (%top :initarg :top :reader top)
   ;; A hash table from functions to sets of interior-nodes
   ;; such that each interior-node has that function as its CODE
   (%dag-nodes :initarg :dag-nodes :reader dag-nodes)))

(defmethod node-function ((node function-dag)) (top node))

(defclass interior-node (dag-node)
  (;; A set of DAG-NODEs; the NODE-FUNCTION of each parent is
   ;; the owner of ENCLOSE.
   (%parents :initarg :parents :reader parents :type set)
   (%enclose :initarg :enclose :reader enclose :type enclose)))

(defmethod node-function ((node dag-node)) (cleavir-bir:code (enclose node)))

(defun build-function-dag-from-set (top set)
  (check-type top cleavir-bir:function)
  (check-type set cleavir-set:set)
  (let* ((dag-nodes (make-hash-table :test #'eq))
         (root (make-instance 'function-dag
                 :dag-nodes dag-nodes :top top)))
    (setf (gethash top dag-nodes) (cleavir-set:make-set root))
    (cleavir-bir:map-instructions-with-owner-from-set
     (lambda (instruction owner)
       (typecase instruction
         (cleavir-bir:enclose
          (let* ((parents (gethash owner dag-nodes))
                 (node (make-instance 'interior-node
                         :parents parents :enclose instruction)))
            (cleavir-set:doset (parent parents)
              (cleavir-set:nadjoinf (%children parent) node))
            (setf (gethash (cleavir-bir:code instruction) dag-nodes)
                  (cleavir-set:nadjoin
                   node (gethash (cleavir-bir:code instruction) dag-nodes
                                 (cleavir-set:empty-set))))))))
     set)
    root))

(defun build-function-dag (top)
  (build-function-dag-from-set top (cleavir-bir:all-functions top)))

;;; Given a function and a DAG, return a set of all functions that enclose
;;; the function, directly or not.
(defun ancestor-functions (function dag)
  (let ((result (cleavir-set:empty-set)))
    (labels ((aux (node)
               (when (typep node 'interior-node)
                 (cleavir-set:nadjoinf result (node-function node))
                 (cleavir-set:mapset nil #'aux (parents node)))))
      (cleavir-set:mapset nil #'aux (gethash function (dag-nodes dag)))
      result)))
