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

(define-deriver (symbol-value domain:type) (client (symbol))
  (declare (ignore symbol))
  (ctype:single-value (ctype:top client) client))
