(in-package #:cleavir-bir-transformations)

;;; Add LEXICAL onto the environment of ACCESS-FUNCTION and do so
;;; recursively.
(defun close-over (function access-function lexical)
  (unless (eq function access-function)
    (let ((environment (cleavir-bir:environment access-function)))
      (unless (cleavir-set:presentp lexical environment)
        (cleavir-set:nadjoinf environment lexical)
        (cleavir-set:doset (enclose (cleavir-bir:encloses access-function))
          (close-over function (cleavir-bir:function enclose) lexical))
        (cleavir-set:doset (local-call (cleavir-bir:local-calls access-function))
          (close-over function (cleavir-bir:function local-call) lexical))))))

;;; Fill in the environments of every function.
(defun process-captured-variables (module)
  (cleavir-set:doset (function (cleavir-bir:functions module) (values))
    (cleavir-set:doset (variable (cleavir-bir:variables function))
      (cleavir-set:doset (reader (cleavir-bir:readers variable))
        (close-over function (cleavir-bir:function reader) variable))
      (cleavir-set:doset (writer (cleavir-bir:writers variable))
        (close-over function (cleavir-bir:function writer) variable)))
    (cleavir-set:doset (catch (cleavir-bir:catches function))
      (cleavir-set:doset (unwind (cleavir-bir:unwinds catch))
        (close-over function (cleavir-bir:function unwind) catch)))))
