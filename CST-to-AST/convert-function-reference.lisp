(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-global-function-reference (client cst info global-env)
  (declare (ignore client global-env))
  (ast:make-constant-fdefinition-ast
   (env:name info)
   :attributes (env:attributes info) :origin cst))

(defmethod convert-function-reference
    (client cst (info env:global-function-info) env)
  (convert-global-function-reference
   client cst info (env:global-environment env)))

(defmethod convert-function-reference
    (client cst (info env:local-function-info) env)
  (declare (ignore client env))
  (ast:make-lexical-ast (env:identity info) :origin cst))

(defmethod convert-function-reference
    (client cst (info env:global-macro-info) env)
  (declare (ignore client env))
  (error 'function-name-names-global-macro :cst cst))

(defmethod convert-function-reference
    (client cst (info env:local-macro-info) env)
  (declare (ignore client env))
  (error 'function-name-names-local-macro :cst cst))

(defmethod convert-function-reference
    (client cst (info env:special-operator-info) env)
  (declare (ignore client env))
  (error 'function-name-names-special-operator :cst cst))

;;; These are used by (foo ...) forms.
;;; It's useful to distinguish them. For instance, an implementation
;;; may bind non-fbound symbols to a function that signals an error
;;; of type UNDEFINED-FUNCTION, allowing an fboundp check to be skipped.
;;; Other than the inlining, they by default have the same behavior.

(defmethod convert-called-function-reference (client cst info env)
  (when (not (eq (env:inline info) 'cl:notinline))
    (let ((ast (env:ast info)))
      (when ast
        (return-from convert-called-function-reference
          (make-instance 'ast:inline-ast
            :origin cst :body-ast ast)))))
  (convert-global-function-reference
   client cst info (env:global-environment env)))

(defmethod convert-called-function-reference
    (client cst (info env:local-function-info) env)
  (declare (ignore client env))
  (ast:make-lexical-ast (env:identity info) :origin cst))

(defmethod convert-called-function-reference
    (client cst (info env:global-macro-info) env)
  (declare (ignore client env))
  (error 'function-name-names-global-macro :cst cst))

(defmethod convert-called-function-reference
    (client cst (info env:local-macro-info) env)
  (declare (ignore client env))
  (error 'function-name-names-local-macro :cst cst))

(defmethod convert-called-function-reference
    (client cst (info env:special-operator-info) env)
  (declare (ignore client env))
  (error 'function-name-names-special-operator :cst cst))
