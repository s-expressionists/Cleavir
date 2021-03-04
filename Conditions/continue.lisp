(in-package #:cleavir-conditions)

(defvar *consistent-state* t)

(defun consistent-state-p (condition)
  (declare (ignore condition))
  *consistent-state*)

(defmacro with-optionality
    ((restart-name format-control &rest format-arguments) &body body)
  `(restart-case (progn ,@body)
     (,restart-name ()
       :report (lambda (stream)
                 (format stream ,format-control ,@format-arguments))
       :test consistent-state-p
       (values nil t))))

(defmacro with-inconsistent-state ((&rest options) &body body)
  (declare (ignore options)) ; for future expansion
  `(let ((*consistent-state* nil))
     ,@body))
