(in-package #:cleavir-bir)

(defmethod acclimation:report-condition
    ((condition unused-variable) stream (language acclimation:english))
  (let ((v (variable condition)))
    (format stream "The variable ~a is ~a but never used."
            (name v)
            (ecase (use-status v)
              ((nil) "defined")
              ((set) "assigned")))))

(defmethod acclimation:report-condition
    ((condition type-conflict) stream (language acclimation:english))
  (format stream "The derived type of ~a~&is ~a~&but is asserted as ~a~&by ~a."
          (datum condition)
          (asserted-type condition)
          (derived-type condition)
          (asserted-by condition)))
