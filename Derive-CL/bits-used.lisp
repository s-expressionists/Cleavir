(in-package #:cleavir-derive-cl)

(defun %and-bits (low high)
  (cond ((or (not low) (not high)) -1)
        ((= low high) low) ; constant, easy
        ((>= low 0)
         ;; TODO: We could do better here, obviously.
         ;; e.g. for [4, 5], obviously only 5's bits are used.
         ;; But I'm not deep enough into Hacker's Delight right now,
         ;; and anyway, the use is probably marginal.
         (ldb (byte (integer-length high) 0) -1))
        (t -1))) ; also suboptimal

;; intuitively i feel this ought to be the dual/lognot of the above?
;; anyway, since it's less than obvious: We don't need a bit if we know we're
;; logioring with a 1 bit, so for example in (logior x -1) x's bits are unused,
;; and in (logior x -8) all bits are unused but the low 3.
(defun %or-bits (low high)
  (cond ((or (not low) (not high)) -1)
        ((= low high) (lognot low))
        ((<= high 0)
         (lognot (ldb (byte (integer-length low) 0) -1)))
        (t -1)))

(defun and-bits (client type)
  (multiple-value-bind (low high emptyp) (type-integer-bounds client type)
    (if emptyp
        0 ; not an integer, so the logand is an error, so no bits are used
        (%and-bits low high))))
(defun or-bits (client type)
  (multiple-value-bind (low high emptyp) (type-integer-bounds client type)
    (if emptyp 0 (%or-bits low high))))

(define-deriver (logand domain:bits-used)
    (client (&rest ints) domain:bits-used (used &rest ignore))
  (declare (ignore ignore))
  ;; Grab any bit ranges we can for the arguments, and use that to restrict
  ;; the result's used bits.
  (loop for int in (rest-types client ints)
        do (setf used (logand used (and-bits client int)))
        finally (return (domain:values-info client domain:bits-used () () used))))

(define-deriver (logior domain:bits-used)
    (client (&rest ints) domain:bits-used (used &rest ignore))
  (declare (ignore ignore))
  (loop for int in (rest-types client ints)
        do (setf used (logand used (or-bits client int)))
        finally (return (domain:values-info client domain:bits-used () () used))))

(define-deriver (lognot domain:bits-used)
    (client (int) domain:bits-used (used &rest ignore))
  (declare (ignore ignore))
  (domain:values-info client domain:bits-used (list used) () 0))

(define-deriver (mod domain:bits-used)
    (client (number divisor) domain:bits-used (used &rest ignore))
  (declare (ignore ignore number))
  ;; We only need the bits for the modulus, so up to the range of the divisor,
  ;; but including zero. Technically we could be more specific by looking at
  ;; the type of the number, but I don't think it's that important.
  (let ((number-bits
          (multiple-value-bind (low high emptyp) (type-integer-bounds client number)
            (cond (emptyp 0)
                  ((or (not low) (not high)) -1)
                  (t
                   (ldb (byte (max (integer-length low) (integer-length high)) 0)
                        -1))))))
    (domain:values-info client domain:bits-used (list number-bits -1) () 0)))

;;; For basic arithmetic, the low N bits of the result depend only on the low N bits
;;; of the arguments. We could do better than this, using _which_ bits are needed,
;;; but for most purposes it's probably not important.
(define-deriver (+ domain:bits-used)
    (client (&rest summands) domain:bits-used (used &rest ignore))
  (declare (ignore ignore))
  (let ((simple-bits (if (< used 0)
                         -1
                         (ldb (byte (integer-length used) 0) -1))))
    (domain:values-info client domain:bits-used () () simple-bits)))
(define-deriver (- domain:bits-used)
    (client (&rest summands) domain:bits-used (used &rest ignore))
  (declare (ignore ignore))
  (let ((simple-bits (if (< used 0)
                         -1
                         (ldb (byte (integer-length used) 0) -1))))
    (domain:values-info client domain:bits-used () () simple-bits)))
;;; here we could potentially do way better? Since the number of bits from the
;;; arguments just has to SUM to the bits needed in the result, e.g. for (* x y)
;;; if we need the low 32 bits, and we know x is 4, we only need 30 bits of y
;;; since the other two bits of y will inevitably be shifted out.
(define-deriver (* domain:bits-used)
    (client (&rest summands) domain:bits-used (used &rest ignore))
  (declare (ignore ignore))
  (let ((simple-bits (if (< used 0)
                         -1
                         (ldb (byte (integer-length used) 0) -1))))
    (domain:values-info client domain:bits-used () () simple-bits)))
