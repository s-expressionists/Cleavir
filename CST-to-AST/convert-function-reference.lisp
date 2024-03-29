(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-global-function-reference (cst info global-env system)
  (declare (ignore global-env system))
  (ast:make-constant-fdefinition-ast
   (env:name info)
   :attributes (env:attributes info) :origin cst))

(defmethod convert-function-reference
    (cst (info env:global-function-info) env system)
  (convert-global-function-reference
   cst info (env:global-environment env) system))

(defmethod convert-function-reference
    (cst (info env:local-function-info) env system)
  (declare (ignore env system))
  (ast:make-lexical-ast (env:identity info) :origin cst))

(defmethod convert-function-reference
    (cst (info env:global-macro-info) env system)
  (declare (ignore env system))
  (error 'function-name-names-global-macro :cst cst))

(defmethod convert-function-reference
    (cst (info env:local-macro-info) env system)
  (declare (ignore env system))
  (error 'function-name-names-local-macro :cst cst))

(defmethod convert-function-reference
    (cst (info env:special-operator-info) env system)
  (declare (ignore env system))
  (error 'function-name-names-special-operator :cst cst))

;;; These are used by (foo ...) forms.
;;; It's useful to distinguish them. For instance, an implementation
;;; may bind non-fbound symbols to a function that signals an error
;;; of type UNDEFINED-FUNCTION, allowing an fboundp check to be skipped.
;;; Other than the inlining, they by default have the same behavior.

(defmethod convert-called-function-reference (cst info env system)
  (when (not (eq (env:inline info) 'cl:notinline))
    (let ((ast (env:ast info)))
      (when ast
        (return-from convert-called-function-reference
          (make-instance 'ast:inline-ast
            :origin cst :body-ast ast)))))
  (convert-global-function-reference
   cst info (env:global-environment env) system))

(defmethod convert-called-function-reference
    (cst (info env:local-function-info) env system)
  (declare (ignore env system))
  (ast:make-lexical-ast (env:identity info) :origin cst))

(defmethod convert-called-function-reference
    (cst (info env:global-macro-info) env system)
  (declare (ignore env system))
  (error 'function-name-names-global-macro :cst cst))

(defmethod convert-called-function-reference
    (cst (info env:local-macro-info) env system)
  (declare (ignore env system))
  (error 'function-name-names-local-macro :cst cst))

(defmethod convert-called-function-reference
    (cst (info env:special-operator-info) env system)
  (declare (ignore env system))
  (error 'function-name-names-special-operator :cst cst))
