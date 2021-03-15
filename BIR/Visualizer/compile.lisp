(cl:in-package #:cleavir.bir.visualizer)

;;; Environment

(defclass visualizer-environment ()
  ((%environment :reader   environment
                 :initform (sb-c::make-null-lexenv))
   (%optimize    :initarg  :optimize
                 :reader   optimize*)))

(defmethod cleavir-env:variable-info
    ((env visualizer-environment) symbol)
  (cleavir-env:variable-info (environment env) symbol))

(defmethod cleavir-env:function-info ((env visualizer-environment) (sym t))
  (cleavir-env:function-info (environment env) sym))

(defmethod cleavir-env:declarations ((env visualizer-environment))
  (cleavir-env:declarations (environment env)))

(defmethod cleavir-env:type-expand ((env visualizer-environment) type)
  (cleavir-env:type-expand (environment env) type))

(defmethod cleavir-env:has-extended-char-p ((env visualizer-environment))
  (cleavir-env:has-extended-char-p (environment env)))

(defmethod cleavir-env:float-types ((env visualizer-environment))
  (cleavir-env:float-types (environment env)))

(defmethod cleavir-env:upgraded-complex-part-types
    ((env visualizer-environment))
  (cleavir-env:upgraded-complex-part-types (environment env)))

(defmethod cleavir-env:upgraded-array-element-types
    ((env visualizer-environment))
  (cleavir-env:upgraded-array-element-types (environment env)))

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

(defun bir-transformations (module system)
  (let ((phases (list ;; 'cleavir-bir-transformations:module-eliminate-catches
                 ;; 'cleavir-bir-transformations:find-module-local-calls
                 'cleavir-bir-transformations:module-optimize-variables
                 (a:rcurry #'cleavir-bir-transformations:meta-evaluate-module system)
                 ;; cc-bir-to-bmir:reduce-module-typeqs
                 ;; cc-bir-to-bmir:reduce-module-primops
                 'cleavir-bir-transformations:module-generate-type-checks
                 ;; These should happen last since they are like "post passes" which
                 ;; do not modify the flow graph.
                 ;; NOTE: These must come in this order to maximize analysis.
                 ;; 'cleavir-bir-transformations:determine-function-environments
                 'cleavir-bir-transformations:determine-closure-extents
                 'cleavir-bir-transformations:determine-variable-extents)))
    (reduce (lambda (module transform)
              (funcall transform module)
              module)
            phases :initial-value module)))

;;; Form reading and hook into compiler

(defun cst<-string (string)
  (eclector.concrete-syntax-tree:read-from-string string))

(defun module<-cst (cst policy)
  (let* ((system nil)
         (output (make-string-output-stream))
         (bir    (let ((*standard-output* output)
                       (*error-output*    output)
                       (*trace-output*    output))
                   (let* ((environment (make-instance 'visualizer-environment :optimize policy))
                          (ast         (cleavir-cst-to-ast:cst-to-ast
                                        cst environment system)))
                     (cleavir-ast-to-bir:compile-toplevel ast system))))
         (module (bir:module bir))
         (module (bir-transformations module system)))
    (values module
            (let ((string (get-output-stream-string output)))
              (if (a:emptyp string) nil string))
            (with-output-to-string (*standard-output*)
              (cleavir-bir-disassembler:disassemble module)))))

(defun module<-string (string policy)
  (module<-cst (cst<-string string) policy))
