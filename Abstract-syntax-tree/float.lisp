(in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SHORT-FLOAT-ADD-AST.
;;;
;;; This AST is used for adding two values of type SHORT-FLOAT.
;;;
;;; It can be used by an implementation that supports the short-float
;;; data type.  
;;;
;;; Both inputs must be of type SHORT-FLOAT, so in safe code this
;;; restriction has to be checked before this AST is evaluated. 

(defclass short-float-add-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-short-float-add-ast (arg1-ast arg2-ast)
  (make-instance 'short-float-add-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info short-float-add-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast short-float-add-ast))
  (list (arg1-ast ast) (arg2-ast ast)))



