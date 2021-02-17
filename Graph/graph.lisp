(in-package #:cleavir-graph)

(deftype graph-mapper () '(function ((function (t)))))
(deftype node-mapper () '(function ((function (t)) t)))

(defgeneric graph-functions (graph))

(defmacro with-graph ((graph) &body body)
  (let ((root (gensym "ROOT")) (size (gensym "SIZE"))
        (map-nodes (gensym "MAP-NODES"))
        (map-depth-first-preorder (gensym "MAP-DF-PRE"))
        (map-inputs (gensym "MAP-INPUTS")) (map-outputs (gensym "MAP-OUTPUTS"))
        (map-predecessors (gensym "MAP-PREDECESSORS"))
        (map-successors (gensym "MAP-SUCCESSORS")))
  `(multiple-value-bind (,root ,size ,map-nodes ,map-nodes-depth-first-preorder
                         ,map-inputs ,map-outputs
                         ,map-predecessors ,map-successors)
       (graph-functions ,graph)
     (declare (type (function () t) ,root)
              (type (function (t) (integer 0)) ,size)
              (type graph-mapper ,map-nodes ,map-nodes-depth-first-preorder)
              (type node-mapper ,map-inputs  ,map-outputs
                    ,map-predecessors ,map-successors))
     (flet ((root () (funcall ,root))
            (size () (funcall ,size))
            (map-nodes (function) (funcall ,map-nodes))
            (map-nodes-depth-first-preorder (function)
              (funcall ,map-nodes-depth-first-preorder function))
            (depth-first-preorder ()
              (let ((preorder nil))
                (funcall ,map-nodes-depth-first-preorder
                         (lambda (node) (push node preorder)))
                preorder))
            (map-inputs (function node) (funcall ,map-inputs function node))
            (map-outputs (function node) (funcall ,map-outputs function node))
            (map-predecessors (function node)
              (funcall ,map-predecessors function node))
            (map-successors (function node)
              (funcall ,map-successors function node)))
       (declare (inline root size map-nodes map-nodes-depth-first-preorder
                        map-inputs map-outputs
                        map-predecessors map-successors)
                (ignorable #'root #'size
                           #'map-nodes #'map-nodes-depth-first-preorder
                           #'map-inputs #'map-outputs
                           #'map-predecessors #'map-successors))
       ,@body))))

(defmacro do-nodes ((name &optional result) &body body)
  `(block nil
     (map-nodes (lambda (,name) (tagbody ,@body)))
     ,result))
(defmacro do-nodes-depth-first-preorder ((name &optional result) &body body)
  `(block nil
     (map-nodes-depth-first-preorder (lambda (,name) (tagbody ,@body)))
     ,result))

(defmacro do-inputs ((name node &optional result) &body body)
  `(block nil
     (map-inputs (lambda (,name) (tagbody ,@body)) ,node)
     ,result))
(defmacro do-outputs ((name node) &body body)
  `(block nil
     (map-outputs (lambda (,name) (tagbody ,@body)) ,node)
     ,result))
(defmacro do-predecessors ((name node &optional result) &body body)
  `(block nil
     (map-predecessors (lambda (,name) (tagbody ,@body)) ,node)
     ,result))
(defmacro do-successors ((name node &optional result) &body body)
  `(block nil
     (map-successors (lambda (,name) (tagbody ,@body)) ,node)
     ,result))
