(cl:in-package #:cleavir.bir.visualizer)

;;; Environment

(defclass visualizer-environment ()
  ((%environment :reader   environment
                 :initform (sb-c::make-null-lexenv))
   (%optimize    :initarg  :optimize
                 :reader   optimize*)))

(defmethod cleavir-env:variable-info
    (sys (env visualizer-environment) symbol)
  (cleavir-env:variable-info sys (environment env) symbol))

(defmethod cleavir-env:function-info (sys (env visualizer-environment) (sym t))
  (cleavir-env:function-info sys (environment env) sym))

(defmethod cleavir-env:declarations ((env visualizer-environment))
  (cleavir-env:declarations (environment env)))

(defmethod cleavir-env:type-expand ((env visualizer-environment) type)
  (cleavir-env:type-expand (environment env) type))

(defmethod cleavir-compilation-policy:policy-qualities append ((env visualizer-environment))
  (loop :for (quality value) :in (optimize* env)
        :collect `(,quality (integer 0 3) ,value)))

(defmethod cleavir-policy:compute-policy-quality
    (name optimize (environment visualizer-environment))
  (cleavir-policy:compute-policy-quality name optimize (environment environment)))

(defmethod cleavir-env:optimize-info ((environment visualizer-environment))
  (let ((optimize (optimize* environment)))
    (make-instance 'cleavir-env:optimize-info
                   :optimize optimize
                   :policy (cleavir-policy:compute-policy
                            optimize environment))))

;;;

(defun bir-transformations (system module transforms)
  (reduce (lambda (module transform)
            (if (consp transform)       ; KLUDGE
                (funcall (first transform) module system)
                (funcall transform module)) ; not all transforms return the module
            module)
          transforms :initial-value module))

;;; Form reading and hook into compiler

(defun cst<-string (string)
  (eclector.concrete-syntax-tree:read-from-string string))

(defun module<-cst (cst policy transforms)
  (let* ((system :visualizer)
         (output (make-string-output-stream))
         (bir    (let ((*standard-output* output)
                       (*error-output*    output)
                       (*trace-output*    output))
                   (let* ((environment (make-instance 'visualizer-environment :optimize policy))
                          (ast         (cleavir-cst-to-ast:cst-to-ast
                                        cst environment system)))
                     (cleavir-ast-to-bir:compile-toplevel ast system))))
         (module (bir:module bir))
         (module (bir-transformations system module transforms)))
    (values module
            (let ((string (get-output-stream-string output)))
              (if (a:emptyp string) nil string))
            (with-output-to-string (*standard-output*)
              (cleavir-bir-disassembler:display module)))))

(defun module<-string (string policy transforms)
  (module<-cst (cst<-string string) policy transforms))
