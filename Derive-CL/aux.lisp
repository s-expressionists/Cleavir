(in-package #:cleavir-derive-cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Auxiliary functions for working with types etc., used by various derivers.
;;;; Also contains a few generic functions that can be specialized by clients to
;;;; account for wiggle room in the standard.
;;;; GENERALIZED-TRUE is a major one: it indicates the true return value of a
;;;; predicate that is defined only as far as returning a generalized boolean.
;;;; It is likely these functions (e.g. CONSP) return exactly T, which is a
;;;; tighter type than the default (NOT NIL), but implementations have to
;;;; affirmatively indicate so.

(defun distribute (client function type)
  (cond ((ctype:conjunctionp type client)
         (apply #'ctype:conjoin client
                (loop for ty in (ctype:conjunction-ctypes type client)
                      collect (distribute client function ty))))
        ((ctype:disjunctionp type client)
         (apply #'ctype:disjoin client
                (loop for ty in (ctype:disjunction-ctypes type client)
                      collect (distribute client function ty))))
        ((ctype:negationp type client)
         (ctype:negate (distribute client function
                                   (ctype:negation-ctype type client))
                       client))
        (t (funcall function type))))

(defun maybe (client type)
  (ctype:disjoin client type (ctype:member client nil)))

(defgeneric generalized-true (client operator)
  (:method (client operator)
    (declare (ignore operator))
    (ctype:negate (ctype:member client nil) client)))

(defun derive-type-predicate (client operator objtype ctype)
  (ctype:single-value
   (cond ((ctype:subtypep objtype ctype client) (generalized-true client operator))
         ((ctype:disjointp objtype ctype client) (ctype:member client nil))
         (t (ctype:disjoin client (ctype:member client nil)
                           (generalized-true client operator))))
   client))

;;; Given a values info as received in &rest, return a list of all sv infos in it.
;;; Useful for functions that don't care about the structure, like n-ary arithmetic.
(defun rest-infos (client domain values-info)
  (append (domain:values-required client domain values-info)
          (domain:values-optional client domain values-info)
          (list (domain:values-rest client domain values-info))))

;;; Given a values type as received in &rest, return a list of all types in it.
;;; Useful for functions that don't care about the structure, like n-ary arithmetic.
(defun rest-types (client values-ctype)
  ;; or: (rest-info client domain:type values-ctype)
  (append (ctype:values-required values-ctype client)
          (ctype:values-optional values-ctype client)
          (list (ctype:values-rest values-ctype client))))

(defun constant-type-p (client type)
  (and (ctype:member-p client type)
    (= (length (ctype:member-members client type)) 1)))

(defun constant-type-value (client type)
  (first (ctype:member-members client type)))

;;; Get inclusive integer bounds from a type. NIL for unbounded.
;;; Third value is T iff the interval is empty.
;;; FIXME: For integer types we should just normalize away exclusivity at parse
;;; time, really.
(defun range-integer-bounds (client ranget)
  (let ((kind (ctype:range-kind ranget client)))
    (multiple-value-bind (low lxp high hxp) (range-bounds client ranget)
      (case kind
        ((integer)
         (values (if (and low lxp) (1+ low) low) (if (and high hxp) (1- high) high)
                 nil))
        ((rational real)
         (values (if low
                     (multiple-value-bind (clow crem) (ceiling low)
                       (if (and (zerop crem) lxp) (1+ clow) clow))
                     low)
                 (if high
                     (multiple-value-bind (fhigh frem) (floor high)
                       (if (and (zerop frem) hxp) (1- fhigh) fhigh))
                     high)
                 nil))
        (t (values nil nil t))))))

;;; raw intervals are way easier to work with than disjunctions and such.
;;; This function takes an arbitrary type and returns low and high bounds for it as
;;; an integer range, for use in arithmetic functions.
;;; It loses precision, since e.g. (or (integer 3 7) (integer 20 29)) is flattened
;;; to [3, 29]. But what it loses in precision is gained in my sanity.
;;; Also in compilation time since you'd have quadratic blowup working with all those
;;; possible subranges, in general.
;;; Third value is T iff the interval is empty.
(defun type-integer-bounds (client type)
  (cond ((ctype:disjunctionp type client)
         (loop with emptyp = t with min = nil with max = nil
               for type in (ctype:disjunction-ctypes type client)
               do (multiple-value-bind (low high subemptyp)
                      (type-integer-bounds client type)
                    (cond (subemptyp)
                          (emptyp (setf emptyp nil min low max high))
                          (t (cond ((not min))
                                   ((not low) (setf min low))
                                   ((< low min) (setf min low)))
                             (cond ((not max))
                                   ((not high) (setf max high))
                                   ((> high max) (setf max high))))))
               finally (return (values min max emptyp))))
        ((ctype:conjunctionp type client)
         (loop with min = nil with max = nil
               for type in (ctype:conjunction-ctypes type client)
               do (multiple-value-bind (low high subemptyp)
                      (type-integer-bounds client type)
                    (when subemptyp (return (values nil nil t)))
                    (cond ((not low))
                          ((and max (> low max)) (return (values nil nil t)))
                          ((not min) (setf min low))
                          ((> low min) (setf min low)))
                    (cond ((not high))
                          ((and min (< high min)) (return (values nil nil t)))
                          ((not max) (setf max high))
                          ((< high max) (setf max high))))
               finally (return (values min max nil))))
        ((ctype:rangep type client) (range-integer-bounds client type))
        (t (values nil nil nil))))
