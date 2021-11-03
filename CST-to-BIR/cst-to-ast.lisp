(cl:in-package #:cleavir-cst-to-bir)

(defvar *current-module*)

(defun cst-to-bir (lambda-expression-cst environment system)
  (let ((lambda-list-cst (cst:second lambda-expression-cst))
        (body-cst (cst:rest (cst:rest lambda-expression-cst)))
        (*current-module* (make-instance 'bir:module))
        (bir:*policy* nil))
    (convert-code lambda-list-cst body-cst environment system
                  :origin (cst:source lambda-expression-cst))
    *current-module*))

#+(or)
(defun cst-to-bir (cst environment system)
  (let (#+(or)(*subforms-are-top-level-p* t)
	  #+(or)(*compile-time-too* nil))
    (convert cst environment system)))
