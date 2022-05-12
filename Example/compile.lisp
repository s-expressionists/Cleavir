(in-package #:cleavir-example)

(defun cst->ast (cst)
  (cst-to-ast:cst-to-ast cst *environment* *system*))

(defun ast->bir (ast)
  (ast-to-bir:compile-toplevel ast *system*))

(defun cst->bir (cst)
  (ast->bir (cst->ast cst)))

(defun transform1 (bir phase)
  (ecase phase
    ((:eliminate-come-froms)
     (bir-transformations:module-eliminate-come-froms bir))
    ((:local-calls)
     (bir-transformations:find-module-local-calls bir))
    ((:optimize-variables)
     (bir-transformations:module-optimize-variables bir))
    ((:meta-evaluate)
     (bir-transformations:meta-evaluate-module bir *system*))
    ((:generate-type-checks)
     (bir-transformations:module-generate-type-checks bir *system*))
    ((:extents)
     (bir-transformations:determine-function-environments bir)
     (bir-transformations:determine-closure-extents bir)
     (bir-transformations:determine-variable-extents bir)))
  bir)

(defparameter *phases*
  '(:eliminate-come-froms :local-calls :optimize-variables
    :meta-evaluate :generate-type-checks :extents))

(defun transform (bir &optional (phases *phases*))
  (loop for phase in phases do (transform1 bir phase))
  bir)

(defun frontend (cst)
  (transform (bir:module (cst->bir cst))))
