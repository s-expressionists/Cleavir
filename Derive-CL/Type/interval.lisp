(in-package #:cleavir-derive-cl)

;;; Utilities for working with real number intervals, which are generally easier
;;; than the types we extract them from.

(defstruct (interval (:constructor make-interval (low high)))
  ;; nil means unbounded. list means exclusive.
  (low nil :type (or null real (cons real null)))
  (high nil :type (or null real (cons real null))))

(defun make-unbounded-interval () (make-interval nil nil))
(defun make-empty-interval () (make-interval '(0) '(0)))

(defun bound-parts (finite-bound)
  (if (consp finite-bound)
      (values (car finite-bound) t)
      (values finite-bound nil)))

(defun finite-bound-binop (binop fb1 fb2)
  (multiple-value-bind (b1 bxp1) (bound-parts fb1)
    (multiple-value-bind (b2 bxp2) (bound-parts fb2)
      (let ((r (funcall binop b1 b2)))
        (if (or bxp1 bxp2) (list r) r)))))

(defun finite-bound-unop (unop fb)
  (multiple-value-bind (b bxp) (bound-parts fb)
    (let ((r (funcall unop b)))
      (if bxp (list r) r))))

;;; Comparison

;; Return -1 if the number is below the interval, 1 if above, 0 if in the interval.
(defun interval-num-compare (interval num)
  (let ((low (interval-low interval)) (high (interval-high interval)))
    (cond ((and low (if (consp low) (>= (car low) num) (> low num))) -1)
          ((and high (if (consp high) (<= (car high) num) (< high num))) 1)
          (t 0))))

(defun split-interval (interval where)
  (ecase (interval-num-compare interval where)
    ((-1) (values nil interval))
    ((1) (values interval nil))
    ((0)
     (values (make-interval (interval-low interval) where)
             (make-interval where (interval-high interval))))))

;;; Return the smallest interval including both input intervals.
;;; If the inputs do not intersect, this will not be a strict join, e.g.
;;; [0,7] and [10, 17] merge to [0, 17]
(defun interval-merge (i1 i2)
  (labels ((lbmin (b1 b2)
             (cond ((not b1) b1)
                   ((not b2) b2)
                   (t (multiple-value-bind (b1 xp1) (bound-parts b1)
                        (multiple-value-bind (b2 xp2) (bound-parts b2)
                          (if (and xp1 xp2) (list (min b1 b2)) (min b1 b2)))))))
           (hbmax (b1 b2)
             (cond ((not b1) b1)
                   ((not b2) b2)
                   (t (multiple-value-bind (b1 xp1) (bound-parts b1)
                        (multiple-value-bind (b2 xp2) (bound-parts b2)
                          (if (and xp1 xp2) (list (max b1 b2)) (max b1 b2))))))))
    (make-interval (lbmin (interval-low i1) (interval-low i2))
                   (hbmax (interval-high i1) (interval-high i2)))))

;;; Addition and subtraction

(defun interval-negate (interval)
  (make-interval
   (let ((high (interval-high interval)))
     (if high (finite-bound-unop #'- high) nil))
   (let ((low (interval-low interval)))
     (if low (finite-bound-unop #'- low) nil))))

;;; Multiplication

;; Multiply two intervals that are both positive, i.e. have lower bounds
;; that are at least zero.
(defun interval*-both-pos (i1 i2)
  (make-interval
   ;; the lower bounds are necessarily finite.
   (finite-bound-binop #'* (interval-low i1) (interval-low i2))
   (let ((h1 (interval-high i1)) (h2 (interval-high i2)))
     (if (and h1 h2)
         (finite-bound-binop #'* h1 h2)
         nil))))

;; Multiply a positive interval by any interval.
(defun interval*-1-pos (i1 i2)
  (multiple-value-bind (i2L i2H) (split-interval i2 0)
    (let ((rL (and i2L (interval-negate
                        (interval*-both-pos i1 (interval-negate i2L)))))
          (rH (and i2H (interval*-both-pos i1 i2H))))
      (if rL
          (if rH
              (interval-merge rL rH)
              rL)
          rH))))

(defun interval* (i1 i2)
  (multiple-value-bind (i1L i1H) (split-interval i1 0)
    (multiple-value-bind (i2L i2H) (split-interval i2 0)
      (let* ((i1L (and i1L (interval-negate i1L)))
             (i2L (and i2L (interval-negate i2L)))
             (iLL (and i1L i2L (interval*-both-pos i1L i2L)))
             (iLH (and i1L i2H (interval-negate (interval*-both-pos i1L i2H))))
             (iHL (and i1H i2L (interval-negate (interval*-both-pos i1H i2L))))
             (iHH (and i1H i2H (interval*-both-pos i1H i2H))))
        (labels ((pim (i1 i2)
                   (cond ((not i1) i2)
                         ((not i2) i1)
                         (t (interval-merge i1 i2)))))
          (pim iLL (pim iLH (pim iHL iHH))))))))

;;; Division

;;; Compute the reciprocal of an all-positive interval, i.e. low must be finite
;;; and greater than zero (or 0 exclusive).
(defun interval-reciprocal-+ (interval)
  (multiple-value-bind (low lxp) (bound-parts (interval-low interval))
    (multiple-value-bind (high hxp) (bound-parts (interval-high interval))
      (make-interval (cond ((not high) '(0))
                           (hxp (list (/ high)))
                           (t (/ high)))
                     (cond ((not lxp) (/ low))
                           ((zerop low) nil)
                           (t (list (/ low))))))))

;;; Returns two values - one an all-negative interval and one all-positive.
;;; Either can be NIL if empty.
;;; This is necessary because the reciprocal of a zero-crossing interval has
;;; a hole in the middle, i.e. is not strictly an interval itself.
(defun interval-reciprocal (interval)
  (multiple-value-bind (low lxp) (bound-parts (interval-low interval))
    (multiple-value-bind (high hxp) (bound-parts (interval-high interval))
      (values
       (if (and low (> low 0))
           nil ; no interval
           (make-interval
            (cond ((or (not high) (>= high 0)) nil)
                  (hxp (list (/ high)))
                  (t (/ high)))
            (cond ((not low) '(0))
                  (lxp (list (/ low)))
                  (t (/ low)))))
       (if (and high (< high 0))
           nil
           (make-interval
            (cond ((not high) '(0))
                  (hxp (list (/ high)))
                  (t (/ high)))
            (cond ((or (not low) (<= low 0)) nil)
                  (lxp (list (/ low)))
                  (t (/ low)))))))))

(defun interval-truncate (interval)
  (let ((low (bound-parts (interval-low interval)))
        (high (bound-parts (interval-high interval))))
    (make-interval (if low (truncate low) nil)
                   (if high (truncate high) nil))))

(defun interval-floor (interval)
  (multiple-value-bind (low lxp) (bound-parts (interval-low interval))
    (declare (ignore lxp)) ; FIXME: Could this be open...? It's not on SBCL
    (multiple-value-bind (high hxp) (bound-parts (interval-high interval))
      (make-interval (if low (floor low) nil)
                     (cond ((not high) nil)
                           ;; floor of (10.0) is 9, not 10
                           (hxp (multiple-value-bind (quo rem) (floor high)
                                  (if (zerop rem) (1- quo) quo)))
                           (t (floor high)))))))

(defun interval-ceiling (interval)
  (multiple-value-bind (low lxp) (bound-parts (interval-low interval))
    (multiple-value-bind (high hxp) (bound-parts (interval-high interval))
      (declare (ignore hxp)) ; ditto comment from interval-floor
      (make-interval (cond ((not low) nil)
                           (lxp (multiple-value-bind (quo rem) (ceiling low)
                                  (if (zerop rem) (1+ quo) quo)))
                           (t (ceiling low)))
                     (if high (ceiling high) nil)))))

(defun floor-remainder (dividend divisor)
  ;; Technically the range of the remainder can change based on the range of the
  ;; dividend, like when the dividend range is smaller than a known divisor,
  ;; but that probably doesn't come up enough to be interesting.
  ;; For FLOOR, the remainder always has the same sign as the divisor.
  ;; Note that we don't check for division by zero here. That's mostly out of
  ;; laziness, but it should be okay since the quotient type does more checking.
  (declare (ignore dividend))
  (make-interval (let ((low (bound-parts (interval-low divisor))))
                   (cond ((not low) low)
                         ((>= low 0) 0)
                         ;; these are always exclusive.
                         ;; e.g (floor x -2) never has a remainder of -2.
                         (t (list low))))
                 (let ((high (bound-parts (interval-high divisor))))
                   (cond ((not high) high)
                         ((<= high 0) 0)
                         (t (list high))))))

(defun ceiling-remainder (dividend divisor)
  (declare (ignore dividend))
  ;; The remainder has the opposite sign of the divisor.
  (make-interval (let ((high (bound-parts (interval-high divisor))))
                   (cond ((not high) high)
                         ((<= high 0) 0)
                         (t (list (- high)))))
                 (let ((low (bound-parts (interval-low divisor))))
                   (cond ((not low) low)
                         ((>= low 0) 0)
                         (t (list (- low)))))))

(defun truncate-remainder (dividend divisor)
  ;; The remainder has the same sign as the dividend. A bit trickier.
  ;; First, get the divisor bound with the largest magnitude.
  (let* ((low (bound-parts (interval-low divisor)))
         (high (bound-parts (interval-high divisor)))
         (max (if (or (not low) (not high))
                  nil
                  (max (abs low) (abs high)))))
    (let ((low (interval-low dividend)) (high (interval-high dividend)))
      (cond ((and low (>= low 0))
             ;; Dividend is positive, so the remainder must be.
             (make-interval 0 (if max (list max) nil)))
            ((and high (<= high 0))
             ;; Dividend is negative, so the remainder must be.
             (make-interval (if max (list (- max)) nil) 0))
            ;; Dividend is either sign, so we don't know.
            (max (make-interval (list (- max)) (list max)))
            (t (make-interval nil nil))))))
