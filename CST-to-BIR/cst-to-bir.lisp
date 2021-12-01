(cl:in-package #:cleavir-cst-to-bir)

(defun cst-to-bir (lambda-expression-cst environment system)
  (let ((lambda-list-cst (cst:second lambda-expression-cst))
        (body-cst (cst:rest (cst:rest lambda-expression-cst)))
        (*current-module* (make-instance 'bir:module))
        (bir:*origin* (cst:source lambda-expression-cst))
        (bir:*policy* (env:environment-policy environment)))
    (convert-code lambda-list-cst body-cst environment system)))
