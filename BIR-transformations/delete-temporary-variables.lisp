(in-package #:cleavir-bir-transformations)

;;; Attempt to optimize a variable.
(defun optimize-variable (variable)
  ;; Unreferenced variable can be deleted.
  (when (set:empty-set-p (bir:readers variable))
    (set:mapset nil #'bir:delete-instruction (bir:writers variable))))

(defun function-optimize-variables (function)
  (set:mapset nil #'optimize-variable (bir:variables function)))

(defun module-optimize-variables (module)
  (bir:map-functions #'function-optimize-variables module))
