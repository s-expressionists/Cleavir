(cl:in-package #:cleavir.bir.visualizer)

;;; Environment

(defclass visualizer-environment ()
  ((%environment :initarg :environment
                 :reader  environment)
   (%optimize    :initarg :optimize
                 :reader  optimize*)))

(defmethod trucler:describe-optimize
    (client (environment visualizer-environment))
  (apply #'make-instance 'trucler:optimize-description
         (loop for optimize in (optimize* environment)
               for (quality value) = (if (listp optimize)
                                         optimize
                                         (list optimize 3))
               collect (intern (string quality) :keyword)
               collect value)))

(macrolet ((def (name &rest lambda-list)
             `(defmethod ,name ,(loop for item in lambda-list
                                      collect
                                      (if (eq item 'environment)
                                          '(environment visualizer-environment)
                                          item))
                (,name ,@(loop for item in lambda-list
                               append
                               (cond ((eq item 'environment)
                                      '((environment environment)))
                                     ((position item lambda-list-keywords)
                                      '())
                                     (t (list item))))))))
  (def trucler:describe-variable client environment name)
  (def trucler:describe-function client environment name)
  (def trucler:describe-block client environment name)
  (def trucler:describe-tag client environment tag)
  (def trucler:describe-declarations client environment)
  (def trucler:global-environment client environment)
  (def trucler:global-environment-p client environment)
  (def trucler:augment-with-variable-description client environment variable-description)
  (def trucler:augment-with-function-description client environment function-description)
  (def trucler:augment-with-block-description client environment block-description)
  (def trucler:augment-with-tag-description client environment tag-description)
  (def trucler:augment-with-optimize-description client environment optimize-description)
  (def trucler:add-lexical-variable client environment symbol &optional identity)
  (def trucler:add-local-special-variable client environment symbol)
  (def trucler:add-local-symbol-macro client environment symbol expansion)
  (def trucler:add-local-function client environment function-name &optional identity)
  (def trucler:add-local-macro client environment symbol expander)
  (def trucler:add-block client environment symbol &optional identity)
  (def trucler:add-tag client environment tag &optional identity)
  (def trucler:add-variable-type client environment symbol type)
  (def trucler:add-function-type client environment function-name type)
  (def trucler:add-variable-ignore client environment symbol ignore)
  (def trucler:add-function-ignore client environment function-name ignore)
  (def trucler:add-variable-dynamic-extent client environment symbol)
  (def trucler:add-function-dynamic-extent client environment function-name)
  (def trucler:add-inline client environment function-name inline)
  (def trucler:add-inline-data client environment function-name inline-data)
  (def trucler:add-speed client environment value)
  (def trucler:add-compilation-speed client environment value)
  (def trucler:add-debug client environment value)
  (def trucler:add-safety client environment value)
  (def trucler:add-space client environment value)
  (def trucler:restrict-for-macrolet-expander client environment)
  (def cleavir-cst-to-ast:type-expand client type environment))

;;;

(defun bir-transformations (client module transforms)
  (reduce (lambda (module transform)
            (if (consp transform)       ; KLUDGE
                (funcall (first transform) client module)
                (funcall transform module)) ; not all transforms return the module
            module)
          transforms :initial-value module))

;;; Form reading and hook into compiler

(defun cst<-string (string)
  (eclector.concrete-syntax-tree:read-from-string string))

(defvar *client*)
(defvar *global-environment*)

(defun module<-cst (cst policy transforms)
  (let* ((client *client*)
         (output (make-string-output-stream))
         (bir    (let ((*standard-output* output)
                       (*error-output*    output)
                       (*trace-output*    output))
                   (let* ((environment (make-instance 'visualizer-environment
                                         :environment *global-environment*
                                         :optimize policy))
                          (ast         (cleavir-cst-to-ast:cst-to-ast
                                        client cst environment)))
                     (cleavir-ast-to-bir:compile-toplevel client ast))))
         (module (bir:module bir))
         (module (bir-transformations client module transforms)))
    (values module
            (let ((string (get-output-stream-string output)))
              (if (a:emptyp string) nil string))
            (with-output-to-string (*standard-output*)
              (cleavir-bir-disassembler:display module)))))

(defun module<-string (string policy transforms)
  (module<-cst (cst<-string string) policy transforms))
