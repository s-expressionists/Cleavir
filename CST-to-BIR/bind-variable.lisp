(cl:in-package #:cleavir-cst-to-bir)

(defmethod bind-variable (variable-cst (info env:lexical-variable-info)
                          initvals inserter system)
  (declare (ignore system))
  (let* ((bir:*origin* (cst:source variable-cst))
         (var (env:identity info))
         (leti (make-instance 'bir:leti
                 :inputs initvals :outputs (list var))))
    (adjoin-variable inserter var)
    (insert inserter leti)
    (setf (bir:binder var) leti))
  (values))

;;; TODO: Default method for special bindings
(defmethod bind-variable (variable-cst (info env:special-variable-info)
                          initvals inserter system)
  (declare (ignore system))
  (let* ((name (convert-constant variable-cst inserter))
         (bname (symbolicate (cst:raw variable-cst) '#:-bound))
         (during (make-iblock inserter :name bname))
         (bind (make-instance 'cc-bir:bind
                 :inputs (list* (first name) initvals)
                 :next (list during))))
    (setf (bir:dynamic-environment during) bind)
    (terminate inserter bind)
    (begin inserter during))
  (values))
