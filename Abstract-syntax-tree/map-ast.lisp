(cl:in-package #:cleavir-ast)

(defun map-ast-depth-first-preorder (function ast)
  (labels ((visit (ast)
             (funcall function ast)
             (cleavir-ast:map-children #'visit ast)))
    (visit ast)))
