(in-package #:cleavir-derive-cl)

(define-deriver (cons domain:type) (client (car cdr))
  (declare (ignore car cdr))
  ;; We can't forward the argument types into the cons type, since we don't
  ;; know if this cons will be mutated. So we just return the CONS type.
  ;; This is useful so that the compiler understands that CONS definitely
  ;; returns a CONS and it does not need to insert any runtime checks.
  (let ((top (ctype:top client)))
    (ctype:single-value (ctype:cons top top client) client)))

(defun type-car (client type)
  (distribute
   client
   (lambda (type)
     (cond ((ctype:consp type client)
            (ctype:cons-car type client))
           ((ctype:member-p client type)
            (apply #'ctype:member client
                   (loop for o in (ctype:member-members client type)
                         when (listp o)
                           collect (car o))))
           (t (ctype:top client))))
   type))
(defun type-cdr (client type)
  (distribute
   client
   (lambda (type)
     (cond ((ctype:consp type client)
            (ctype:cons-cdr type client))
           ((ctype:member-p client type)
            (apply #'ctype:member client
                   (loop for o in (ctype:member-members client type)
                         when (listp o)
                           collect (cdr o))))
           (t (ctype:top client))))
   type))

(defmacro defcr-type (name &rest ops)
  `(define-deriver (,name domain:type) (client (obj))
     (ctype:single-value
      ,(labels ((rec (ops)
                  (if ops
                      `(,(first ops) client ,(rec (rest ops)))
                      'obj)))
         (rec ops))
      client)))

(defcr-type car type-car)
(defcr-type cdr type-cdr)
(defcr-type caar type-car type-car)
(defcr-type cadr type-car type-cdr)
(defcr-type cdar type-cdr type-car)
(defcr-type cddr type-cdr type-cdr)
(defcr-type caaar type-car type-car type-car)
(defcr-type caadr type-car type-car type-cdr)
(defcr-type cadar type-car type-cdr type-car)
(defcr-type caddr type-car type-cdr type-cdr)
(defcr-type cdaar type-cdr type-car type-car)
(defcr-type cdadr type-cdr type-car type-cdr)
(defcr-type cddar type-cdr type-cdr type-car)
(defcr-type cdddr type-cdr type-cdr type-cdr)
(defcr-type caaaar type-car type-car type-car type-car)
(defcr-type caaadr type-car type-car type-car type-cdr)
(defcr-type caadar type-car type-car type-cdr type-car)
(defcr-type caaddr type-car type-car type-cdr type-cdr)
(defcr-type cadaar type-car type-cdr type-car type-car)
(defcr-type cadadr type-car type-cdr type-car type-cdr)
(defcr-type caddar type-car type-cdr type-cdr type-car)
(defcr-type cadddr type-car type-cdr type-cdr type-cdr)
(defcr-type cdaaar type-cdr type-car type-car type-car)
(defcr-type cdaadr type-cdr type-car type-car type-cdr)
(defcr-type cdadar type-cdr type-car type-cdr type-car)
(defcr-type cdaddr type-cdr type-car type-cdr type-cdr)
(defcr-type cddaar type-cdr type-cdr type-car type-car)
(defcr-type cddadr type-cdr type-cdr type-car type-cdr)
(defcr-type cdddar type-cdr type-cdr type-cdr type-car)
(defcr-type cddddr type-cdr type-cdr type-cdr type-cdr)

(defcr-type rest type-cdr)
(defcr-type first type-car)
(defcr-type second type-car type-cdr)
(defcr-type third type-car type-cdr type-cdr)
(defcr-type fourth type-car type-cdr type-cdr type-cdr)
(defcr-type fifth type-car type-cdr type-cdr type-cdr type-cdr)
(defcr-type sixth type-car type-cdr type-cdr type-cdr type-cdr type-cdr)
(defcr-type seventh type-car
  type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr)
(defcr-type eighth type-car
  type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr)
(defcr-type ninth type-car
  type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr)
(defcr-type tenth type-car
  type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr type-cdr
  type-cdr)

(define-deriver (list domain:type) (client (&rest args))
  (let ((top (ctype:top client)))
    (ctype:single-value
     (multiple-value-bind (min max) (values-type-minmax client args)
       (cond ((> min 0)
              ;; can't be more detailed because conses are mutable, as with cl:cons
              (ctype:cons top top client))
             ((and max (zerop max)) (ctype:member client nil))
             (t (ctype:disjoin client (ctype:member client nil)
                               (ctype:cons top top client)))))
     client)))

(define-deriver (list* domain:type) (client (arg &rest args))
  (let ((top (ctype:top client)))
    (ctype:single-value
     (multiple-value-bind (min max) (values-type-minmax client args)
       (cond ((> min 0) (ctype:cons top top client))
             ((and max (zerop max)) arg)
             (t (ctype:disjoin client arg (ctype:cons top top client)))))
     client)))
