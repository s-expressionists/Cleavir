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

(define-deriver (symbol-function domain:type) (client (symbol))
  (declare (ignore symbol))
  (ctype:single-value (ctype:function-top client) client))
(define-deriver ((setf symbol-function) domain:type) (client (new symbol))
  (declare (ignore symbol))
  (ctype:single-value new client))

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

(define-deriver (character domain:type) (client (object))
  (declare (ignore object))
  (ctype:single-value (ctype:character client) client))

(macrolet ((defchar (name)
             `(define-deriver (,name domain:type) (client (character))
                (declare (ignore character))
                (ctype:single-value (ctype:character client) client))))
  (defchar char-upcase) (defchar char-downcase))

(macrolet ((defcharcmp (name)
             `(define-deriver (,name domain:type) (client (&rest characters))
                (declare (ignore characters))
                (ctype:single-value (generalized-boolean client ',name) client)))
           (defcharcmps (&rest names)
             `(progn ,@(loop for name in names collect `(defcharcmp ,name)))))
  (defcharcmps char= char/= char< char> char<= char>=
    char-equal char-not-equal char-lessp char-greaterp
    char-not-greaterp char-not-lessp))

(macrolet ((defcharpred (name)
             `(define-deriver (,name domain:type) (client (character))
                (declare (ignore character))
                (ctype:single-value (generalized-boolean client ',name) client)))
           (defcharpreds (&rest names)
             `(progn ,@(loop for name in names collect `(defcharpred ,name)))))
  (defcharpreds alpha-char-p graphic-char-p upper-case-p lower-case-p both-case-p))

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
