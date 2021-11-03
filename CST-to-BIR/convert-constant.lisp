(in-package #:cleavir-cst-to-bir)

(defun convert-constant (constant-cst inserter)
  (let ((const (bir:constant-in-module (cst:raw constant-cst) *current-module*))
        (constref-out (make-instance 'bir:output)))
    (insert inserter
            (make-instance 'bir:constant-reference
              :inputs (list const) :outputs (list constref-out)))
    (list constref-out)))
