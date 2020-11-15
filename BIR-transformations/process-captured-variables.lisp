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

;;; Determine the extent of closures. We mark closures created by
;;; ENCLOSE instructions as dynamic extent if all of its uses are
;;; calls with the DX-call attribute. Make sure we only mark closures
;;; as dynamic extent and do not try to mark a function as indefinite
;;; extent, since there may be an explicit dynamic extent declaration
;;; on the function which we should preserve.
(defun dynamic-extent-analyze-closures (module)
  (flet ((safe-call (call)
           (cleavir-attributes:has-boolean-attribute-p
            (cleavir-bir:attributes call)
            :dx-call)))
    (cleavir-set:doset (function (cleavir-bir:functions module))
      (cleavir-set:doset (enclose (cleavir-bir:encloses function))
        (unless (cleavir-bir:unused-p enclose)
          (let ((use (cleavir-bir:use enclose)))
            (typecase use
              (cleavir-bir:call
               (when (safe-call use)
                 (setf (cleavir-bir:extent enclose) :dynamic)))
              (cleavir-bir:writevar
               (let ((variable (first (cleavir-bir:outputs use)))
                     (safe t))
                 (cleavir-set:doset (reader (cleavir-bir:readers variable))
                   (unless (cleavir-bir:unused-p reader)
                     (let ((use (cleavir-bir:use reader)))
                       (typecase use
                         (cleavir-bir:call
                          (setq safe (safe-call use)))
                         (t (setq safe nil))))))
                 (when safe
                   (setf (cleavir-bir:extent enclose) :dynamic)))))))))))
