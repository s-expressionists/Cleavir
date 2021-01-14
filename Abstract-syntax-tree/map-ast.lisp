(cl:in-package #:cleavir-ast)

(defun map-ast-depth-first-preorder (function ast)
  (labels ((visit (ast)
             (funcall function ast)
             (dolist (child (children ast))
               (visit child))))
    (visit ast)))
