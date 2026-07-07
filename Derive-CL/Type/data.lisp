(in-package #:cleavir-derive-cl)

(defun false (client)
  (ctype:member client nil))

(defun true (client)
  (ctype:negate (ctype:member client nil) client))

(defun derive-eq/l (client arg1 arg2 eq1 eq2)
  (ctype:single-value
   (cond ((eql eq1 eq2) (true client))
         ((ctype:disjointp arg1 arg2 client) (false client))
         (t (ctype:top client)))
   client))

(define-deriver (eq domain:type) (client (a1 a2) domain:equivalence (e1 e2))
  (derive-eq/l client a1 a2 e1 e2))
(define-deriver (eql domain:type) (client (a1 a2) domain:equivalence (e1 e2))
  (derive-eq/l client a1 a2 e1 e2))

(define-deriver (identity domain:type) (client (arg))
  (ctype:single-value arg client))

(define-deriver (values domain:type) (client (&rest args))
  ;;(declare (ignore client))
  args)
