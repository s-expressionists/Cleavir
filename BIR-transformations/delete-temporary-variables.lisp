(in-package #:cleavir-bir-transformations)

;;;; Here a "temporary variable" is one with one writer and one reader, and
;;;; which is local. This can obviously be replaced. E.g. "v := f(x); g(v)"
;;;; can obviously be transformed into "g(f(x))".
;;;; That is what this transform does.

(defun delete-variable (variable)
  (let ((writer (cleavir-set:arb (cleavir-bir:writers variable)))
        (reader (cleavir-set:arb (cleavir-bir:readers variable))))
    (cleavir-bir:delete-transmission writer reader)))

(defun temporary-variable-p (variable)
  (and (not (cleavir-bir:closed-over-p variable))
       (= (cleavir-set:size (cleavir-bir:writers variable)) 1)
       (= (cleavir-set:size (cleavir-bir:readers variable)) 1)))

(defun delete-temporary-variables (module)
  (cleavir-set:doset (function (cleavir-bir:functions module))
    (cleavir-set:doset (var (cleavir-bir:variables function))
      (when (temporary-variable-p var)
        (delete-variable var)))))
