(in-package #:cleavir-example)

(defun cst->ast (cst)
  (cst-to-ast:cst-to-ast cst *environment* *system*))

(defun ast->bir (ast)
  (ast-to-bir:compile-toplevel ast *system*))

(defun cst->bir (cst)
  (ast->bir (cst->ast cst)))

(defun abstract-interpret (module)
  (let* ((strategy (make-instance 'abstract-interpreter:sequential-slots))
         (system *system*)
         (atype (make-instance 'abstract-interpreter:asserted-type
                  :system system))
         (dtype (make-instance 'abstract-interpreter:derived-type
                  :system system))
         (attr (make-instance 'abstract-interpreter:attribute))
         (reach (make-instance 'abstract-interpreter:reachability))
         (ra (make-instance 'abstract-interpreter:reachability->data
               :input reach :output atype))
         (rd (make-instance 'abstract-interpreter:reachability->data
               :input reach :output dtype))
         (rat (make-instance 'abstract-interpreter:reachability->data
                :input reach :output attr))
         (tr (make-instance 'abstract-interpreter:type->reachability
               :input dtype :output attr))
         (kc (make-instance 'abstract-interpreter:known-call-channel
               :output dtype :other attr :flower #'derive-return-type))
         (product (make-instance 'abstract-interpreter:product
                    :domains (list atype dtype attr reach)
                    :channels (list ra rd rat tr kc))))
    (abstract-interpreter:interpret-module strategy product module)))

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
    ((:abstract-interpret)
     (abstract-interpret bir))
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
