(in-package #:cleavir-derive-cl)

(defmacro define-deriver-type-predicate (name client type)
  (let ((object (gensym "OBJECT")))
    `(define-deriver (,name domain:type) (,client (,object))
       (ctype:single-value
        (let ((type ,type))
          (cond ((ctype:subtypep ,object type ,client)
                 (generalized-true ,client ',name))
                ((ctype:disjointp ,object type ,client) (false ,client))
                (t (generalized-boolean client ',name))))
        ,client))))

(defun class-type (client class-name)
  (ctype:class (find-class class-name) client))

(define-deriver-type-predicate functionp client (ctype:function-top client))
(define-deriver-type-predicate compiled-function-p client
  (ctype:compiled-function client))

(define-deriver-type-predicate symbolp client (class-type client 'symbol))
(define-deriver-type-predicate keywordp client (ctype:keyword client))

(define-deriver-type-predicate packagep client (class-type client 'package))

(define-deriver-type-predicate numberp client (class-type client 'number))
(define-deriver-type-predicate complexp client
  (ctype:complex (ctype:top client) client))
(define-deriver-type-predicate realp client (ctype:range 'real '* '* client))
(define-deriver-type-predicate rationalp client (ctype:range 'rational '* '* client))
(define-deriver-type-predicate floatp client (ctype:range 'float '* '* client))
(define-deriver-type-predicate integerp client (ctype:range 'integer '* '* client))

(define-deriver-type-predicate random-state-p client
  (class-type client 'random-state))

(define-deriver-type-predicate characterp client (ctype:character client))

;; We could be more specific here, since standard-char-p of a non-character
;; is actually an error, but this is a valid approximation.
(define-deriver-type-predicate standard-char-p client (ctype:standard-char client))

(define-deriver-type-predicate consp client
  (ctype:cons (ctype:top client) (ctype:top client) client))
(define-deriver-type-predicate atom client
  (ctype:negate (ctype:cons (ctype:top client) (ctype:top client) client) client))
(define-deriver-type-predicate listp client
  (ctype:disjoin client
                 (ctype:cons (ctype:top client) (ctype:top client) client)
                 (ctype:member client nil)))
(define-deriver-type-predicate endp client (ctype:member client nil))
(define-deriver-type-predicate null client (ctype:member client nil))

(define-deriver-type-predicate arrayp client (ctype:array '* '* 'array client))
(define-deriver-type-predicate vectorp client (ctype:array '* '(*) 'array client))
(define-deriver-type-predicate bit-vector-p client
  (ctype:array 'bit '(*) 'array client))
(define-deriver-type-predicate simple-bit-vector-p client
  (ctype:array 'bit '(*) 'simple-array client))

(define-deriver-type-predicate simple-string-p client
  (ctype:string '* 'simple-array client))
(define-deriver-type-predicate stringp client (ctype:string '* 'array client))

(define-deriver-type-predicate hash-table-p client (class-type client 'hash-table))

(define-deriver-type-predicate pathnamep client (class-type client 'pathname))

(define-deriver-type-predicate streamp client (class-type client 'stream))
