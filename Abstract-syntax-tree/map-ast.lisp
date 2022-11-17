(cl:in-package #:cleavir-ast)

(defun map-ast-depth-first-preorder (function ast)
  "Call FUNCTION on AST and all of its CHILDREN, in depth-first order."
  (labels ((visit (ast)
             (funcall function ast)
             (cleavir-ast:map-children #'visit ast)))
    (visit ast)))
