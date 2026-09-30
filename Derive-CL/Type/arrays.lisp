(in-package #:cleavir-derive-cl)

;;; what it says on the tin. If they are adjustable, we can't infer the
;;; dimensions of an array, since they can be adjusted.
;;; Hypothetically we could be more specific here - is an array that's
;;; made with :adjustable nil and :fill-pointer something adjustable?
;;; But the complexity isn't needed unless some implementation is weird.
(defgeneric simple-arrays-actually-adjustable-p (client)
  (:method (client) (declare (ignore client)) t))

(define-deriver-type-predicate arrayp client (ctype:array '* '* 'array client))
(define-deriver-type-predicate vectorp client (ctype:array '* '(*) 'array client))
(define-deriver-type-predicate bit-vector-p client
  (ctype:array 'bit '(*) 'array client))
(define-deriver-type-predicate simple-bit-vector-p client
  (ctype:array 'bit '(*) 'simple-array client))

#+(or)
(define-deriver (make-array domain:type)
    (client (dimensions &key (element-type (ctype:member client 't))
                        (adjustable (ctype:member client 'nil))
                        (fill-pointer (ctype:member client 'nil))
                        (displaced-to (ctype:member client 'nil))))
  (flet ((dimension-spec (type)
           (if (and (ctype:rangep type client)
                    (eq (ctype:range-kind type client) 'integer))
               (multiple-value-bind (low lxp) (ctype:range-low type client)
                 ;; ignore exclusivity since integer types should be normalized
                 (declare (ignore lxp))
                 (multiple-value-bind (high hxp) (ctype:range-high type client)
                   (declare (ignore hxp))
                   (if (= low high) low nil)))
               nil)))
    (let* ((top (ctype:top client))
           (null (ctype:member client 'nil))
           (simplicity (if (and (ctype:subtypep adjustable null client)
                                (ctype:subtypep fill-pointer null client)
                                (ctype:subtypep displaced-to null client))
                           'simple-array
                           'array))
           (dimensions-valid-p
             ;; see above note about adjustability.
             ;; Note that we can infer the _rank_, since that can't be adjusted.
             (and (eq simplicity 'simple-array)
                  (not (simple-arrays-actually-adjustable-p client))))
           (list (ctype:disjoin client null (ctype:cons top top client)))
           (idimensions
             (cond ((ctype:subtypep dimensions null client) ())
                   ((dimension-spec dimensions)
                    (if dimensions-valid-p
                        (list (dimension-spec dimensions))
                        '(*)))
                   ((ctype:subtypep dimensions (ctype:negate list client) client)
                    ;; if the dimensions isn't a list, it must be a designator,
                    ;; i.e. a number, so this is a vector.
                    '(*))
                   ((ctype:consp dimensions client)
                    (loop for cons = dimensions then cdr
                          for car = (ctype:cons-car cons client)
                          for cdr = (ctype:cons-cdr cons client)
                          collect (if dimensions-valid-p
                                      (or (dimension-spec car) '*)
                                      '*)
                          until (ctype:subtypep cdr null client)
                          when (not (ctype:consp cdr client))
                            ;; list of unknown length (nil covered by above)
                            return '*))
                   (t '*)))
           (uaet (if (constant-type-p client element-type)
                     (handler-case
                         (ctype:upgraded-array-element-type
                          (parse client
                                 (constant-type-value client element-type))
                          client)
                       (error () '*))
                     '*))
           (array
             (ctype:array uaet idimensions simplicity client)))
      (ctype:single-value array client))))

(defun type-aet (client type)
  (if (ctype:arrayp type client)
      (ctype:array-element-type type client)
      (ctype:top client)))

(define-deriver (aref domain:type) (client (array &rest indices))
  ;; TODO: return bottom if indices are invalid
  (declare (ignore indices))
  (ctype:single-value (type-aet client array) client))
(define-deriver ((setf aref) domain:type) (client (new array &rest indices))
  (declare (ignore array indices))
  (ctype:single-value new client))

(define-deriver (row-major-aref domain:type) (client (array index))
  (declare (ignore index))
  (ctype:single-value (type-aet client array) client))
(define-deriver ((setf row-major-aref) domain:type) (client (new array index))
  (declare (ignore array index))
  (ctype:single-value new client))

(defun type-array-dimensions (client type)
  (if (ctype:arrayp type client)
      (ctype:array-dimensions type client)
      '*))

(defgeneric array-dimension-limit-value (client)
  (:method (client) (declare (ignore client)) '*))

(define-deriver (array-dimension domain:type) (client (array axis))
  (ctype:single-value
   (let ((dimensions (type-array-dimensions client array)))
     (flet ((give-up ()
              (ctype:range 'integer 0
                           (let ((limit (array-dimension-limit-value client)))
                             (if (eq limit '*) limit (1- limit)))
                           client)))
       (if (and (constant-type-p client axis) (not (eql dimensions '*)))
           (let* ((axis (constant-type-value client axis))
                  (dim (nth axis dimensions)))
             (if (eql dim '*)
                 (give-up)
                 (ctype:range 'integer dim dim client)))
           (give-up))))
   client))

(defun type-array-rank (client type)
  (let ((dims (type-array-dimensions client type)))
    (if (eq dims '*)
        dims
        (length dims))))

(defgeneric array-rank-limit-value (client)
  (:method (client) (declare (ignore client)) '*))

(define-deriver (array-rank domain:type) (client (array))
  (ctype:single-value (let ((rank (type-array-rank client array)))
                        (if (eq rank '*)
                            (ctype:range 'integer rank rank client)
                            (ctype:range 'integer 0 (array-rank-limit-value client)
                                         client)))
                      client))
