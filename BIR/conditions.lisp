(in-package #:cleavir-bir)

(define-condition unused-variable (conditions:origin
                                   conditions:program-style-warning)
  ((%variable :initarg :variable :reader variable)))

(define-condition type-conflict (conditions:origin conditions:program-warning)
  ((%derived-type :initarg :derived-type :reader derived-type)
   (%asserted-type :initarg :asserted-type :reader asserted-type)
   (%datum :initarg :datum :reader datum)
   (%asserted-by :initarg :asserted-by :reader asserted-by)))

;;; BIR failing the verifier means that something is wrong with the compiler,
;;; not the source code. So this is NOT a program-error.
(define-condition verification-failed (acclimation:condition error)
  ((%module :initarg :module :reader module)
   (%function-problems :initarg :function-problems :reader function-problems)
   (%module-problems :initarg :module-problems :reader module-problems)))

;;; Similarly, this indicates a problem in the verifier itself.
(define-condition verification-error (acclimation:condition error)
  ((%module :initarg :module :reader module)
   (%original-condition :initarg :original-condition
                        :reader original-condition)))
