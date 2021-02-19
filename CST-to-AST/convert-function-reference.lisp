(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-global-function-reference (cst info global-env system)
  (declare (ignore global-env system))
  (let ((source (cst:source cst)))
    (cleavir-ast:make-fdefinition-ast
     (cleavir-ast:make-constant-ast (trucler:name info) :origin source)
     ;;:attributes (cleavir-env:attributes info)
     :origin source)))

(defmethod convert-function-reference
    (cst (info trucler:global-function-description) env system)
  (convert-global-function-reference
   cst info (trucler:global-environment system env) system))

(defmethod convert-function-reference
    (cst (info trucler:local-function-description) env system)
  (declare (ignore env system))
  (cleavir-ast:make-lexical-ast (trucler:identity info)
    :origin (cst:source cst)))

(defmethod convert-function-reference
    (cst (info trucler:global-macro-description) env system)
  (declare (ignore env system))
  (error 'function-name-names-global-macro :cst cst))

(defmethod convert-function-reference
    (cst (info trucler:local-macro-description) env system)
  (declare (ignore env system))
  (error 'function-name-names-local-macro :cst cst))

(defmethod convert-function-reference
    (cst (info trucler:special-operator-description) env system)
  (declare (ignore env system))
  (error 'function-name-names-special-operator :cst cst))

;;; These are used by (foo ...) forms.
;;; It's useful to distinguish them. For instance, an implementation
;;; may bind non-fbound symbols to a function that signals an error
;;; of type UNDEFINED-FUNCTION, allowing an fboundp check to be skipped.
;;; Other than the inlining, they by default have the same behavior.

(defmethod convert-called-function-reference (cst info env system)
  (when (not (eq (trucler:inline info) 'cl:notinline))
    (let ((ast (trucler:inline-data info)))
      (when ast
        (return-from convert-called-function-reference ast))))
  (convert-global-function-reference
   cst info (trucler:global-environment env) system))

(defmethod convert-called-function-reference
    (cst (info trucler:local-function-description) env system)
  (declare (ignore env system))
  (cleavir-ast:make-lexical-ast (trucler:identity info)
    :origin (cst:source cst)))

(defmethod convert-called-function-reference
    (cst (info trucler:global-macro-description) env system)
  (declare (ignore env system))
  (error 'function-name-names-global-macro :cst cst))

(defmethod convert-called-function-reference
    (cst (info trucler:local-macro-description) env system)
  (declare (ignore env system))
  (error 'function-name-names-local-macro :cst cst))

(defmethod convert-called-function-reference
    (cst (info trucler:special-operator-description) env system)
  (declare (ignore env system))
  (error 'function-name-names-special-operator :cst cst))
