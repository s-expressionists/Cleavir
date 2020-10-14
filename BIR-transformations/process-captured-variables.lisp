(in-package #:cleavir-bir-transformations)

;;; Add VARIABLE onto the environment of ACCESS-FUNCTION and do so
;;; recursively.
(defun close-over-variable (function access-function variable)
  (let ((environment (cleavir-bir:environment access-function)))
    (unless (or (eq function access-function)
                (cleavir-set:presentp variable environment))
      (cleavir-set:nadjoinf environment variable)
      (cleavir-set:doset (enclose (cleavir-bir:encloses access-function))
        (close-over-variable function (cleavir-bir:function enclose) variable))
      (cleavir-set:doset (local-call (cleavir-bir:local-calls access-function))
        (close-over-variable function (cleavir-bir:function local-call) variable)))))

;;; Fill in the environments of every function.
(defun process-captured-variables (ir)
  (cleavir-set:doset (function (cleavir-bir:functions (cleavir-bir:module ir)) (values))
    (cleavir-set:doset (variable (cleavir-bir:variables function))
      (cleavir-set:doset (reader (cleavir-bir:readers variable))
        (close-over-variable function (cleavir-bir:function reader) variable))
      (cleavir-set:doset (writer (cleavir-bir:writers variable))
        (close-over-variable function (cleavir-bir:function writer) variable)))))
