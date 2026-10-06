(in-package #:cleavir-derive-cl)

(define-deriver-type-predicate simple-string-p client
  (ctype:string '* 'simple-array client))
(define-deriver-type-predicate stringp client (ctype:string '* 'array client))

(defun derive-char (client string index)
  (declare (ignore index))
  (ctype:single-value
   (let ((character (ctype:character client)))
     (distribute
      client
      (lambda (type)
        (if (ctype:arrayp type client)
            ;; technically we could just return the element type,
            ;; since char on a non string is probably going to err (thus NIL type),
            ;; but deriving char as returning a non-character would be weird
            ;; An implementation could also technically define CHAR to do
            ;; something else, since char on a non-string is UB, but if they do
            ;; they should just not use this deriver. That's too much freedom.
            (let ((aet (ctype:array-element-type type client)))
              (if (ctype:disjointp client aet character)
                  (ctype:bottom client)
                  aet))
            character))
      string))
   client))

(define-deriver (char domain:type) (client (string index))
  (derive-char client string index))
(define-deriver (schar domain:type) (client (string index))
  (derive-char client string index))

(defun type-designated-string (client designator-type)
  (distribute
   client
   (lambda (type)
     (cond ((ctype:arrayp type client) type)
           ;; FIXME
           ;; could at least be simple-string but that's not technically required
           (t (ctype:string '* 'array client))))
   designator-type))

(define-deriver (string domain:type) (client (designator))
  (ctype:single-value (type-designated-string client designator) client))

(macrolet ((defcase (name)
             `(define-deriver (,name domain:type) (client (string &key start end))
                (declare (ignore start end))
                (ctype:single-value (type-designated-string client string) client)))
           (defcases (&rest names)
             `(progn ,@(loop for name in names collect `(defcase ,name)))))
  (defcases string-upcase string-downcase string-capitalize))
(macrolet ((defcase (name)
             `(define-deriver (,name domain:type) (client (string &key start end))
                (declare (ignore start end))
                (ctype:single-value string client)))
           (defcases (&rest names)
             `(progn ,@(loop for name in names collect `(defcase ,name)))))
  (defcases nstring-upcase nstring-downcase nstring-capitalize))

(defun type-designated-string-dimless (client type)
  (distribute
   client
   (lambda (type)
     (cond ((ctype:arrayp type client)
            (let ((dims (ctype:array-dimensions type client)))
              (if (or (eq dims '*) (= (length dims) 1))
                  ;; we do assume that the implementation doesn't return a different
                  ;; element type, which I guess it technically could?
                  (ctype:array (ctype:array-element-type type client)
                               '(*)
                               ;; probably is a simple array, FIXME
                               (ctype:array-simplicity type client)
                               client)
                  (ctype:bottom client))))
           ;; FIXME other designated types go here
           (t (ctype:string '* 'array client))))
   type))
(macrolet ((deftrim (name)
             `(define-deriver (,name domain:type) (client (bag string))
                (declare (ignore bag))
                (ctype:single-value (type-designated-string-dimless client string)
                                    client)))
           (deftrims (&rest names)
             `(progn ,@(loop for name in names collect `(deftrim ,name)))))
  (deftrims string-trim string-left-trim string-right-trim))

(define-deriver (make-string domain:type)
    (client (size &key (initial-element (ctype:character client))
                  (element-type (ctype:member client 'character))))
  (declare (ignore initial-element))
  (ctype:single-value
   (let ((len (if (ctype:rangep size client)
                  (multiple-value-bind (low lxp) (ctype:range-low size client)
                    (multiple-value-bind (high hxp)
                        (ctype:range-high size client)
                      (if (and (not lxp) (not hxp) (= low high))
                          low
                          '*)))
                  '*)))                           
     (cond ((and (ctype:member-p client element-type)
                 (equal (ctype:member-members client element-type) '(character)))
            (ctype:array (ctype:character client) (list len) 'simple-array client))
           ((and (ctype:member-p client element-type)
                 (equal (ctype:member-members client element-type) '(base-char)))
            (ctype:array (ctype:base-char client) (list len) 'simple-array client))
           (t (ctype:string (list len) 'simple-array client))))
   client))
