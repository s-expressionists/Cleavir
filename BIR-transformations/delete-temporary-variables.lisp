(in-package #:cleavir-bir-transformations)

;;;; Here a "temporary variable" is one with one writer and one reader, and
;;;; which is local. This  can obviously be replaced. E.g. "v := f(x); g(v)"
;;;; can obviously be transformed into "g(f(x))".
;;;; That is what this transform does.

(defun delete-variable (variable)
  (declare (optimize debug))
  (let* ((writer (cleavir-set:arb (cleavir-bir:writers variable)))
         (source (first (cleavir-bir:inputs writer)))
         (reader (cleavir-set:arb (cleavir-bir:readers variable))))
    (cleavir-bir:delete-instruction writer)
    (cleavir-bir:replace-computation reader source)))

(defun temporary-variable-p (variable)
  (and (eq (cleavir-bir:extent variable) :local)
       (= (cleavir-set:size (cleavir-bir:writers variable)) 1)
       (= (cleavir-set:size (cleavir-bir:readers variable)) 1)))

(defun delete-temporary-variables-from-set (fset)
  (cleavir-set:doset (funct fset)
    (cleavir-set:doset (var (cleavir-bir:variables funct))
      (when (temporary-variable-p var)
        (delete-variable var)))))

(defun delete-temporary-variables (ir)
  (delete-temporary-variables-from-set
   (cleavir-bir:all-functions ir)))
