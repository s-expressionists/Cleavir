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

(defmethod acclimation:report-condition
    ((condition verification-failed) stream (language acclimation:english))
  (handler-case (let ((*standard-output* stream))
                  (cleavir-bir-disassembler:display (module condition)))
    (error (e)
      (format stream "~&! Error while disassembling module:~@<  ~@;~a~:>" e)))
  ;; This format string has some complicated bits, so here's a word of
  ;; explanation. MODULE-PROBLEMS is a list of "problems". FUNCTION-PROBLEMS
  ;; is a list of (function problem*). A "problem" is a list of a format
  ;; control and a list of format arguments, suitable for use with ~?.
  ;; What ~<~{  ~@;~:@{~a~%~}~:> does is: Pop one format argument and use it as
  ;; the list in a pprint logical block with per-line prefix "  ". This
  ;; argument is a list of problems.
  ;; Iterate over each problem passing it to ~?, and then print newline.
  ;; So given for example problems = (("~hello ~2,'0d" (9)) ("g" ()))
  ;; you get as output:
  ;;   hello 09
  ;;   g
  (format stream "~&BIR verification failed. The IR is in an inconsistent state.
This probably indicates a problem in the compiler; please report it.
~@[~%Problems pertaining to the entire module:~%~<  ~@;~@{~a~%~}~:>~]
~:{~&Problems pertaining to function ~a:~%~@<  ~@;~@{~a~%~}~:>~}"
          (module-problems condition)
          (function-problems condition)))

(defmethod acclimation:report-condition
    ((condition verification-error) stream (language acclimation:english))
  (format stream "BIR verification failed due to the verifier signaling an unexpected error.
This probably indicates a problem in the compiler; please report it.
Error:~%~@<  ~@;~a~:>"
          (original-condition condition)))
