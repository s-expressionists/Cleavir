(in-package #:cleavir-bir-transformations)

;;; Attempt to optimize a variable.
(defun optimize-variable (variable)
  (let ((readers (cleavir-bir:readers variable))
        (writers (cleavir-bir:writers variable)))
    ;; Unreferenced variable can be deleted.
    (when (cleavir-set:empty-set-p readers)
      (cleavir-set:doset (writer writers)
        (cleavir-bir:delete-instruction writer))
      (return-from optimize-variable))
    ;; Local variable with one reader and one writer can be substituted away,
    (when (and (not (cleavir-bir:closed-over-p variable))
               (cleavir-bir:immutablep variable)
               (= (cleavir-set:size readers) 1))
      (let ((writer (cleavir-bir:binder variable))
            (reader (cleavir-set:arb (cleavir-bir:readers variable))))
        ;; We're only allowed to substitute away the binding when the
        ;; variable's reader does not have a stronger type assertion
        ;; on it than the bound value, so as to not lose the type
        ;; assertion.
        (when (cleavir-ctype:subtypep
               (cleavir-bir::%asserted-type (first (cleavir-bir:inputs writer)))
               (cleavir-bir::%asserted-type reader)
               nil)
          (cleavir-bir:delete-transmission writer reader))))
    ;; Variable bound to constant can get propagated.
    (when (cleavir-bir:immutablep variable)
      (let* ((writer (cleavir-set:arb (cleavir-bir:writers variable)))
             (input (first (cleavir-bir:inputs writer))))
        (typecase input
          (cleavir-bir:constant-reference
           (let ((constant (first (cleavir-bir:inputs input))))
             (cleavir-set:doset (reader (cleavir-bir:readers variable))
               (change-class reader 'cleavir-bir:constant-reference
                 :inputs (list constant)))
             (cleavir-bir:delete-instruction writer)
             (cleavir-bir:delete-instruction input)))
          (t))))))

(defun module-optimize-variables (module)
  (cleavir-set:doset (function (cleavir-bir:functions module))
    (cleavir-set:doset (variable (cleavir-bir:variables function))
      (optimize-variable variable))))
