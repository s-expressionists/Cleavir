(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-global-function-reference
    (client cst description global-env)
  (ast:make-constant-fdefinition-ast
   (trucler:name description)
   :attributes (attributes client description global-env) :origin cst))

(defmethod convert-function-reference
    (client cst (description trucler:global-function-description) env)
  (convert-global-function-reference
   client cst description (trucler:global-environment client env)))

(defmethod convert-function-reference
    (client cst (description trucler:local-function-description) env)
  (declare (ignore client env))
  (ast:make-lexical-ast (trucler:identity description) :origin cst))

(defmethod convert-function-reference
    (client cst (description trucler:global-macro-description) env)
  (declare (ignore client env))
  (error 'function-name-names-global-macro :cst cst))

(defmethod convert-function-reference
    (client cst (description trucler:local-macro-description) env)
  (declare (ignore client env))
  (error 'function-name-names-local-macro :cst cst))

(defmethod convert-function-reference
    (client cst (description trucler:special-operator-description) env)
  (declare (ignore client env))
  (error 'function-name-names-special-operator :cst cst))

;;; These are used by (foo ...) forms.
;;; It's useful to distinguish them. For instance, an implementation
;;; may bind non-fbound symbols to a function that signals an error
;;; of type UNDEFINED-FUNCTION, allowing an fboundp check to be skipped.
;;; Other than the inlining, they by default have the same behavior.

(defmethod convert-called-function-reference (client cst description env)
  (when (not (eq (trucler:inline description) 'cl:notinline))
    (let ((ast (trucler:inline-data description)))
      (when ast
        (return-from convert-called-function-reference
          (make-instance 'ast:inline-ast
            :origin cst :body-ast ast)))))
  (convert-global-function-reference
   client cst description (trucler:global-environment client env)))

(defmethod convert-called-function-reference
    (client cst (description trucler:local-function-description) env)
  (declare (ignore client env))
  (ast:make-lexical-ast (trucler:identity description) :origin cst))

(defmethod convert-called-function-reference
    (client cst (description trucler:global-macro-description) env)
  (declare (ignore client env))
  (error 'function-name-names-global-macro :cst cst))

(defmethod convert-called-function-reference
    (client cst (description trucler:local-macro-description) env)
  (declare (ignore client env))
  (error 'function-name-names-local-macro :cst cst))

(defmethod convert-called-function-reference
    (client cst (description trucler:special-operator-description) env)
  (declare (ignore client env))
  (error 'function-name-names-special-operator :cst cst))
