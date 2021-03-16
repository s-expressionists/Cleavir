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
          (derived-type condition)
          (asserted-type condition)
          (asserted-by condition)))

(defun group-problems (problems)
  ;; Group by subject. Also, put function problems before iblock problems, and
  ;; both before instruction problems.
  (let ((table (make-hash-table :test #'eq)))
    (dolist (problem problems)
      (push problem (gethash (subject problem) table)))
    (let ((list (loop for k being the hash-keys of table
                        using (hash-value v)
                      collect (list* k v))))
      (sort list (lambda (s1 s2)
                   (etypecase s1
                     (function t)
                     (iblock (typep s2 'instruction))
                     (instruction nil)))
            :key #'car))))

(defmethod acclimation:report-condition
    ((condition verification-failed) stream (language acclimation:english))
  (macrolet ((dis (thing what)
               `(handler-case (let ((*standard-output* stream))
                                (cleavir-bir-disassembler:display ,thing))
                  (error (e)
                    (format stream
                            ,(concatenate 'string
                                          "~&! Error while disassembling "
                                          what
                                          ":~@<  ~@;~a~:>")
                            e)))))
    (cleavir-bir-disassembler:with-disassembly ()
      (dis (module condition) "module")
      (format stream "~&BIR verification failed. The IR is in an inconsistent state.
This probably indicates a problem in the compiler; please report it.
~@[~%Problems pertaining to the entire module:~%~<  ~@;~@{~a~%~}~:>~]"
              (module-problems condition))
      (when (function-problems condition)
        (loop for (function . problems) in (function-problems condition)
              for grouped = (group-problems problems)
              do (format stream "~&Problems pertaining to function ~a:~%"
                         function)
                 (pprint-logical-block (stream grouped :per-line-prefix "  ")
                   (loop
                     (pprint-exit-if-list-exhausted)
                     (destructuring-bind (subject . problems)
                         (pprint-pop)
                       (if (typep subject 'instruction)
                           (dis subject "instruction")
                           (format stream "~&    ~a" subject))
                       (fresh-line stream)
                       (dolist (problem problems)
                         (apply #'format stream
                                (problem-format-control problem)
                                (problem-format-arguments problem))
                         (terpri stream))))))))))

(defmethod acclimation:report-condition
    ((condition verification-error) stream (language acclimation:english))
  (format stream "BIR verification failed due to the verifier signaling an unexpected error.
This probably indicates a problem in the compiler; please report it.
Error:~%~@<  ~@;~a~:>"
          (original-condition condition)))
