(in-package #:cleavir-cst-to-bir)

(defmethod convert-global-function-reference
    (cst info inserter global-env system)
  (declare (ignore global-env system))
  (let* ((source (cst:source cst))
         (name (env:name info))
         (cr (convert-constant (cst:cst-from-expression
                                name :source source)
                               inserter))
         (fdo (make-instance 'bir:output
                :name name :attributes (env:attributes info))))
    (insert inserter (make-instance 'bir:primop
                       :info (cleavir-primop-info:info 'fdefinition)
                       :inputs cr :outputs (list fdo)))
    (list fdo)))

(defmethod convert-function-reference
    (cst (info env:global-function-info) inserter env system)
  (convert-global-function-reference
   cst info inserter (env:global-environment env) system))

(defmethod convert-function-reference
    (cst (info env:local-function-info) inserter env system)
  ;; TODO: Could we just use the function datum directly rather than
  ;; going through a variable at all?
  (declare (ignore cst env system))
  (let ((var (env:identity info))
        (rv-out (make-instance 'bir:output :name (env:name info))))
    (insert inserter (make-instance 'bir:readvar
                       :inputs (list var) :outputs (list rv-out)))
    (list rv-out)))

(defmethod convert-function-reference
    (cst (info env:global-macro-info) inserter env system)
  (declare (ignore inserter env system))
  (error 'function-name-names-global-macro :cst cst))

(defmethod convert-function-reference
    (cst (info env:local-macro-info) inserter env system)
  (declare (ignore inserter env system))
  (error 'function-name-names-local-macro :cst cst))

(defmethod convert-function-reference
    (cst (info env:special-operator-info) inserter env system)
  (declare (ignore inserter env system))
  (error 'function-name-names-special-operator :cst cst))

;;; These are used by (foo ...) forms.
;;; It's useful to distinguish them. For instance, an implementation
;;; may bind non-fbound symbols to a function that signals an error
;;; of type UNDEFINED-FUNCTION, allowing an fboundp check to be skipped.
;;; Other than the inlining, they by default have the same behavior.

(defmethod convert-called-function-reference
    (cst (info env:global-function-info) inserter env system)
  #+(or) ; TODO
  (when (not (eq (env:inline info) 'cl:notinline))
    (let ((ast (env:ast info)))
      (when ast
        (return-from convert-called-function-reference ast))))
  (convert-global-function-reference
   cst info inserter (env:global-environment env) system))

(defmethod convert-called-function-reference
    (cst (info env:local-function-info) inserter env system)
  (declare (ignore cst env system))
  (let ((var (env:identity info))
        (rv-out (make-instance 'bir:output :name (env:name info))))
    (insert inserter (make-instance 'bir:readvar
                       :inputs (list var) :outputs (list rv-out)))
    (list rv-out)))

(defmethod convert-called-function-reference
    (cst (info env:global-macro-info) inserter env system)
  (declare (ignore inserter env system))
  (error 'function-name-names-global-macro :cst cst))

(defmethod convert-called-function-reference
    (cst (info env:local-macro-info) inserter env system)
  (declare (ignore inserter env system))
  (error 'function-name-names-local-macro :cst cst))

(defmethod convert-called-function-reference
    (cst (info env:special-operator-info) inserter env system)
  (declare (ignore inserter env system))
  (error 'function-name-names-special-operator :cst cst))
