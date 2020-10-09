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

;; Find all calls to a function and mark them as local calls.

;; FIXME: Right now, this runs on functions which are explicitly not
;; closures, so this needs to happen after p-c-v. This is rather
;; suboptimal, since the true power of local calls lies in eliding
;; heap allocated closures. Otherwise we can run this straight away
;; and have p-c-v do lambda lifting on local calls.
(defun find-local-calls (function)
  (cleavir-set:doset (enclose (cleavir-bir:encloses function))
    (let ((use (cleavir-bir:use enclose))
          (external-reference-p nil))
      (when (cleavir-set:empty-set-p (cleavir-bir:environment function))
        (typecase use
          (cleavir-bir:call
           (cond ((eq use (cleavir-bir:callee use))
                  (change-class use 'cleavir-bir:local-call)
                  (cleavir-bir:replace-computation enclose function))
                 (t
                  (setq external-reference-p t))))
          (cleavir-bir:writevar
           (let ((variable (first (cleavir-bir:outputs use))))
             (when (cleavir-bir:immutablep variable)
               (cleavir-set:doset (reader (cleavir-bir:readers variable))
                 (let ((use (cleavir-bir:use reader)))
                   (typecase use
                     (cleavir-bir:call
                      (when (or (eq use (cleavir-bir:callee use))
                                (not (member reader (rest (cleavir-bir:inputs use)))))
                        (change-class use 'cleavir-bir:local-call)
                        (cleavir-bir:replace-computation reader function)))
                     (t
                      (setq external-reference-p t)))))))
           (unless external-reference-p
             (cleavir-bir:delete-instruction use))))
        (unless external-reference-p
          (cleavir-bir:delete-computation enclose))))))

(defun local-call-analyze-module (module)
  (cleavir-set:mapset nil #'find-local-calls (cleavir-bir:functions module)))
