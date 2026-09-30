(in-package #:cleavir-derive-cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (8) Structures

;;; copy-structure more or less returns an object with the same type as its argument.
;;; the "less" is that eql-ness is lost, and this turns out to make things kind of a
;;; pain. While we're at it we can at least rule out some non-structure types.
(define-deriver (copy-structure domain:type) (client (structure))
  (ctype:single-value
   (distribute
    client
    (lambda (type)
      (cond ((ctype:member-p client type) (ctype:top client))
            ((ctype:satisfiesp type client) (ctype:top client)) ; who friggin knows
            ((or (ctype:rangep type client) (ctype:complexp type client)
                 (ctype:consp type client))
             (ctype:bottom client))
            (t type)))
    structure)
   client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (10) Symbols

(define-deriver-type-predicate symbolp client (class-type client 'symbol))
(define-deriver-type-predicate keywordp client (ctype:keyword client))

(define-deriver (symbol-value domain:type) (client (symbol))
  (declare (ignore symbol))
  (ctype:single-value (ctype:top client) client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (10) Packages

(define-deriver-type-predicate packagep client (class-type client 'package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (13) Characters

(define-deriver-type-predicate characterp client (ctype:character client))
;; We could be more specific here, since standard-char-p of a non-character
;; is actually an error, but this is a valid approximation.
(define-deriver-type-predicate standard-char-p client (ctype:standard-char client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (18) Hash Tables

(define-deriver-type-predicate hash-table-p client (class-type client 'hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (19) Filenames

(define-deriver-type-predicate pathnamep client (class-type client 'pathname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (21) Streams

(define-deriver-type-predicate streamp client (class-type client 'stream))
