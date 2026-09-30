(in-package #:cleavir-derive-cl)

(defun false (client)
  (ctype:member client nil))

(defun true (client)
  (ctype:negate (ctype:member client nil) client))

(define-deriver-type-predicate functionp client (ctype:function-top client))
(define-deriver-type-predicate compiled-function-p client
  (ctype:compiled-function client))

(defun derive-eq/l (client arg1 arg2 eq1 eq2)
  (ctype:single-value
   (cond ((domain:equivalentp eq1 eq2) (true client))
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

;; (apply foo bar ... baz) may be converted into
;; (multiple-value-call foo (values bar) (values ...) (values-list baz))
;; so we put in some effort here, essentially converting list types to values types.
(defun derive-values-list (client list-type)
  (cond ((ctype:member-p client list-type)
         (let ((members (ctype:member-members client list-type)))
           (cond ((some #'consp members) ; weird, but ok: just give up
                  (ctype:values-top client))
                 ((member nil members)
                  ;; zero values.
                  (ctype:values nil nil (ctype:bottom client) client))
                 (t ; nothing valid
                  (ctype:values-bottom client)))))
        ((ctype:consp list-type client)
         (let* ((vr (derive-values-list (ctype:cons-cdr list-type client) client))
                (req (ctype:values-required vr client))
                (opt (ctype:values-optional vr client))
                (rest (ctype:values-rest vr client)))
           (ctype:values (list* (ctype:cons-car list-type client) req)
                         opt rest client)))
        ((ctype:conjunctionp list-type client)
         (apply #'ctype:values-conjoin client
                (mapcar (lambda (sub) (derive-values-list sub client))
                        (ctype:conjunction-ctypes list-type client))))
        ((ctype:disjunctionp list-type client)
         (apply #'ctype:values-disjoin client
                (mapcar (lambda (sub) (derive-values-list sub client))
                        (ctype:disjunction-ctypes list-type client))))
        (t (ctype:values-top client))))

(define-deriver (values-list domain:type) (client (list))
  (derive-values-list client list))
