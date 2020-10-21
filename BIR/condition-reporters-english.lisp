(in-package #:cleavir-bir)

(defmethod acclimation:report-condition
    ((condition unused-variable) stream (language acclimation:english))
  (let ((v (variable condition)))
    (format stream "The variable ~a is ~a but never used."
            (name v)
            (ecase (use-status v)
              ((nil) "defined")
              ((set) "assigned")))))
