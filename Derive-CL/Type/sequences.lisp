(in-package #:cleavir-derive-cl)

;;; TODO: MAKE-SEQUENCE, MAP, MERGE, CONCATENATE

(defgeneric maximum-list-length (client)
  (:method (client)
    (declare (ignore client))
    '*))

(defgeneric maximum-sequence-length (client)
  (:method (client)
    (let ((arr (array-dimension-limit-value client))
          (list (maximum-list-length client)))
      (if (eq list '*)
          list
          (max arr list)))))

(defun sequence-type (client) (ctype:class (find-class 'sequence) client))

(defun list-type (client)
  (ctype:disjoin client
                 (ctype:cons (ctype:top client) (ctype:top client) client)
                 (ctype:member client nil)))

;;; Given a sequence type, return a type for its elements. Used for ELT etc.
(defun sequence-element-type (client type)
  (distribute
   client
   (lambda (type)
     (cond ((ctype:arrayp type client) (ctype:array-element-type type client))
           ((ctype:member-p client type)
            (if (equal (ctype:member-members client type) '(nil))
                (ctype:bottom client)
                ;; weak
                (ctype:top client)))
           ;; no cons, also weak
           (t (ctype:top client))))
   type))

;;; Given a sequence type, return a sequence of the same overall type but with
;;; details stripped out. In particular keep length if possible but lose element
;;; type of conses. This is used for FILL, MAP-INTO etc.
(defun sequence-type-id (client type)
  (distribute client
              (lambda (ty)
                (cond ((ctype:arrayp ty client) ty)
                      ((ctype:consp ty client)
                       (ctype:cons (ctype:top client) (ctype:top client)
                                   client))
                      ((ctype:member-p client ty)
                       (if (equal (ctype:member-members client ty) '(nil))
                           ty ; nil is easy
                           ;; something weird happening, give up
                           (sequence-type client)))
                      (t (sequence-type client))))
              type))

;;; Like the above but lose length information. Used for functions that return
;;; subsequences.
(defun sequence-type-lengthfree (client type)
  (distribute client
              (lambda (ty)
                (cond ((ctype:arrayp ty client)
                       (ctype:array (ctype:array-element-type ty client)
                                    '(*)
                                    (ctype:array-simplicity ty client)
                                    client))
                      ((ctype:consp ty client) (list-type client))
                      ((ctype:member-p client ty)
                       (if (equal (ctype:member-members client ty) '(nil))
                           (list-type client)
                           (sequence-type client)))
                      (t (sequence-type client))))
              type))

(defun cons-length-bounds (client type)
  ;; could be improved by computing a minimum length and not just a maximum
  (loop for ty = type then (ctype:cons-cdr ty client)
        for len from 0
        if (not (ctype:consp ty client))
          return (values
                  0
                  (if (ctype:member-p client ty)
                      ;; this could be improved slightly by checking for
                      ;; the case of the member not having any proper lists,
                      ;; in which case the result of LENGTH is bottom
                      ;; but why bother
                      (+ len (loop for obj in (ctype:member-members client ty)
                                   when (listp obj)
                                     maximizing (handler-case (length obj)
                                                  (error () 0))))
                      (maximum-list-length client)))))

;;; Min and max bounds for the length of a sequence.
(defun sequence-length-bounds (client type)
  (flet ((hmax (b1 b2)
           (if (or (eq b1 '*) (eq b2 '*)) '* (max b1 b2)))
         (hmin (b1 b2)
           (cond ((eq b1 '*) b2)
                 ((eq b2 '*) b1)
                 (t (min b1 b2)))))
    (cond ((ctype:disjunctionp type client)
           (loop with min = 0 with max = 0
                 for st in (ctype:disjunction-ctypes type client)
                 do (multiple-value-bind (smin smax)
                        (sequence-length-bounds client st)
                      (setf min (min min smin)
                            max (hmax max smax)))
                 finally (return (values min max))))
          ((ctype:conjunctionp type client)
           (loop with min = 0 with max = '*
                 for st in (ctype:conjunction-ctypes type client)
                 do (multiple-value-bind (smin smax)
                        (sequence-length-bounds client st)
                      (setf min (max min smin)
                            max (hmin max smax)))
                 finally (return (values min max))))
          ((ctype:arrayp type client)
           (let ((dims (ctype:array-dimensions type client)))
             (cond ((or (eq dims '*) (equal dims '(*)))
                    (values 0 (array-dimension-limit-value client)))
                   ((= (length dims) 1) (values (first dims) (first dims)))
                   ;; not a vector, but we don't really have a "bottom" here
                   (t (values 0 0)))))
          ((ctype:consp type client) (cons-length-bounds client type))
          ((ctype:member-p client type)
           (loop for obj in (ctype:member-members client type)
                 when (and (typep obj 'sequence)
                           ;; try to rule out improper lists
                           (handler-case (length obj) (error () nil)))
                   minimizing (length obj) into min
                   and maximizing (length obj) into max
                 finally (return (values min max))))
          (t (values 0 (maximum-sequence-length client))))))

(defun sequence-length-max (client type)
  (nth-value 1 (sequence-length-bounds client type)))

;;; Given the type for a result-type specifier, return the best type you can for it.
;;; This is used when inferring the result of CONCATENATE, MAP, etc.
(defun specified-sequence-type (client result-type-type)
  (if (constant-type-p client result-type-type)
      (multiple-value-bind (ctype validp)
          (ctype:approximate-parse
           client (constant-type-value client result-type-type))
        (if validp
            ;; we could check that this is a subtype of SEQUENCE, but we don't
            ;; actually have to, since in that case an error should probably be
            ;; signaled (not sure this is strictly required).
            ctype
            (sequence-type client)))
      (sequence-type client)))

;;;

(define-deriver (concatenate domain:type) (client (result-type &rest seqs))
  (declare (ignore seqs))
  (ctype:single-value (specified-sequence-type client result-type) client))

(define-deriver (copy-seq domain:type) (client (sequence))
  (ctype:single-value (sequence-type-id client sequence) client))

(define-deriver (elt domain:type) (client (sequence index))
  (declare (ignore index))
  (ctype:single-value (sequence-element-type client sequence) client))

(define-deriver (fill domain:type) (client (sequence item &rest keys))
  (declare (ignore item keys))
  (ctype:single-value (sequence-type-id client sequence) client))

(define-deriver (make-sequence domain:type)
    (client (result-type length &key initial-element))
  (declare (ignore length initial-element))
  (ctype:single-value (specified-sequence-type client result-type) client))

(define-deriver (subseq domain:type) (client (sequence start &rest end))
  (declare (ignore start end))
  (ctype:single-value (sequence-type-lengthfree client sequence) client))

(define-deriver (map domain:type) (client (result-type function &rest seqs))
  (declare (ignore function seqs))
  (ctype:single-value (specified-sequence-type client result-type) client))

(define-deriver (map-into domain:type) (client (sequence function &rest seqs))
  (declare (ignore function seqs))
  (ctype:single-value (sequence-type-id client sequence) client))

(define-deriver (merge domain:type) (client (result-type seq1 seq2
                                                         predicate &key key))
  (declare (ignore seq1 seq2 predicate key))
  (ctype:single-value (specified-sequence-type client result-type) client))

(define-deriver (reduce domain:type) (client (function sequence &rest keys))
  (declare (ignore function sequence keys))
  ;; TODO: Look at the function type, I guess
  (ctype:single-value (ctype:top client) client))

(define-deriver (count domain:type) (client (item sequence &rest keys))
  (declare (ignore item keys))
  (ctype:single-value
   (ctype:range 'integer 0 (nth-value 1 (sequence-length-bounds client sequence))
                client)
   client))
(define-deriver (count-if domain:type) (client (predicate sequence &rest keys))
  (declare (ignore predicate keys))
  (ctype:single-value
   (ctype:range 'integer 0 (nth-value 1 (sequence-length-bounds client sequence))
                client)
   client))
(define-deriver (count-if-not domain:type) (client (predicate sequence &rest keys))
  (declare (ignore predicate keys))
  (ctype:single-value
   (ctype:range 'integer 0 (nth-value 1 (sequence-length-bounds client sequence))
                client)
   client))

(define-deriver (length domain:type) (client (sequence))
  (ctype:single-value
   (multiple-value-bind (min max) (sequence-length-bounds client sequence)
     (ctype:range 'integer min max client))
   client))

(define-deriver (reverse domain:type) (client (sequence))
  (ctype:single-value (sequence-type-id client sequence) client))
(define-deriver (nreverse domain:type) (client (sequence))
  (ctype:single-value (sequence-type-id client sequence) client))

(define-deriver (sort domain:type) (client (sequence predicate &rest keys))
  (declare (ignore predicate keys))
  (ctype:single-value (sequence-type-id client sequence) client))
(define-deriver (stable-sort domain:type) (client (sequence predicate &rest keys))
  (declare (ignore predicate keys))
  (ctype:single-value (sequence-type-id client sequence) client))

(define-deriver (find domain:type) (client (item sequence &rest keys))
  (declare (ignore item keys))
  (ctype:single-value (maybe client (sequence-element-type client sequence)) client))
(define-deriver (find-if domain:type) (client (predicate sequence &rest keys))
  (declare (ignore predicate keys))
  (ctype:single-value (maybe client (sequence-element-type client sequence)) client))
(define-deriver (find-if-not domain:type) (client (predicate sequence &rest keys))
  (declare (ignore predicate keys))
  (ctype:single-value (maybe client (sequence-element-type client sequence)) client))

(define-deriver (position domain:type) (client (item sequence &rest keys))
  (declare (ignore item keys))
  (ctype:single-value
   (maybe client
          (ctype:range 'integer 0 (sequence-length-max client sequence) client))
   client))
(define-deriver (position-if domain:type) (client (predicate sequence &rest keys))
  (declare (ignore predicate keys))
  (ctype:single-value
   (maybe client
          (ctype:range 'integer 0 (sequence-length-max client sequence) client))
   client))
(define-deriver (position-if-not domain:type)
    (client (predicate sequence &rest keys))
  (declare (ignore predicate keys))
  (ctype:single-value
   (maybe client
          (ctype:range 'integer 0 (sequence-length-max client sequence) client))
   client))

(define-deriver (search domain:type) (client (subsequence sequence &rest keys))
  (declare (ignore subsequence keys))
  (ctype:single-value
   (maybe client
          (ctype:range 'integer 0 (sequence-length-max client sequence) client))
   client))

(define-deriver (mismatch domain:type) (client (seq1 seq2 &rest keys))
  (declare (ignore seq2 keys))
  (ctype:single-value
   (maybe client
          (ctype:range 'integer 0 (sequence-length-max client seq1) client))
   client))

(define-deriver (replace domain:type) (client (seq1 seq2 &rest keys))
  (declare (ignore seq2 keys))
  (ctype:single-value (sequence-type-id client seq1) client))

(define-deriver (substitute domain:type) (client (new old sequence &rest keys))
  (declare (ignore new old keys))
  (ctype:single-value (sequence-type-id client sequence) client))
(define-deriver (nsubstitute domain:type) (client (new old sequence &rest keys))
  (declare (ignore new old keys))
  (ctype:single-value (sequence-type-id client sequence) client))

(define-deriver (substitute-if domain:type)
    (client (new predicate sequence &rest keys))
  (declare (ignore new predicate keys))
  (ctype:single-value (sequence-type-id client sequence) client))
(define-deriver (substitute-if-not domain:type)
    (client (new predicate sequence &rest keys))
  (declare (ignore new predicate keys))
  (ctype:single-value (sequence-type-id client sequence) client))
(define-deriver (nsubstitute-if domain:type)
    (client (new predicate sequence &rest keys))
  (declare (ignore new predicate keys))
  (ctype:single-value (sequence-type-id client sequence) client))
(define-deriver (nsubstitute-if-not domain:type)
    (client (new predicate sequence &rest keys))
  (declare (ignore new predicate keys))
  (ctype:single-value (sequence-type-id client sequence) client))

(define-deriver (remove domain:type) (client (item sequence &rest keys))
  (declare (ignore item keys))
  (ctype:single-value (sequence-type-lengthfree client sequence) client))
(define-deriver (delete domain:type) (client (item sequence &rest keys))
  (declare (ignore item keys))
  (ctype:single-value (sequence-type-lengthfree client sequence) client))

(define-deriver (remove-if domain:type) (client (predicate sequence &rest keys))
  (declare (ignore predicate keys))
  (ctype:single-value (sequence-type-lengthfree client sequence) client))
(define-deriver (remove-if-not domain:type) (client (predicate sequence &rest keys))
  (declare (ignore predicate keys))
  (ctype:single-value (sequence-type-lengthfree client sequence) client))
(define-deriver (delete-if domain:type) (client (predicate sequence &rest keys))
  (declare (ignore predicate keys))
  (ctype:single-value (sequence-type-lengthfree client sequence) client))
(define-deriver (delete-if-not domain:type) (client (predicate sequence &rest keys))
  (declare (ignore predicate keys))
  (ctype:single-value (sequence-type-lengthfree client sequence) client))

(define-deriver (remove-duplicates domain:type) (client (sequence &rest keys))
  (declare (ignore keys))
  (ctype:single-value (sequence-type-lengthfree client sequence) client))
(define-deriver (delete-duplicates domain:type) (client (sequence &rest keys))
  (declare (ignore keys))
  (ctype:single-value (sequence-type-lengthfree client sequence) client))
