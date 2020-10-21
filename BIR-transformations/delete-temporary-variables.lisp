(in-package #:cleavir-bir-transformations)

;;; Attempt to optimize a variable.
(defun optimize-variable (variable)
  (let ((readers (cleavir-bir:readers variable))
        (writers (cleavir-bir:writers variable)))
    ;; Unreferenced variable can be deleted.
    (when (cleavir-set:empty-set-p readers)
      (cleavir-set:doset (writer writers)
        (cleavir-bir:delete-instruction writer)))
    ;; Local variable with one reader and one writer can be substituted away,
    (when (and (not (cleavir-bir:closed-over-p variable))
               (zerop (1- (cleavir-set:size writers)))
               (zerop (1- (cleavir-set:size readers))))
      (let ((writer (cleavir-set:arb (cleavir-bir:writers variable)))
            (reader (cleavir-set:arb (cleavir-bir:readers variable))))
        (cleavir-bir:delete-transmission writer reader)))
    ;; Variable bound to constant can get propagated.
    #+(or)
    (when (immutablep variable)
      (let ((writer (cleavir-set:arb (cleavir-bir:writers variable))))
        (let ((input (first (inputs writer))))
          ;; We can't do this nicely yet. We'd like to check for a
          ;; constant here and then change the readvar's to references
          ;; to the constant.
          )))))

(defun module-optimize-variables (module)
  (cleavir-set:doset (function (cleavir-bir:functions module))
    (cleavir-set:doset (variable (cleavir-bir:variables function))
      (optimize-variable variable))))
