(in-package #:cleavir-domain)

;;;; Track which bits of an integer are actually used by operations.
;;;; This information could allow, for instance,
;;;; (ldb (byte 32 0) (* x y)) to be compiled to use a machine's modular
;;;; arithmetic instructions, or (mod (expt x y) (1- (ash 1 64))) to use
;;;; modular exponentiation algorithms rather than consing up a huge bignum
;;;; which is then cut down at the end.

(defclass bits-used (values-mixin domain) ())
(defvar bits-used (make-instance 'bits-used))

;; An SV info is just an integer with bits set as an indicator.
;; 1 means the bit may be used, 0 means it is unused.

(defmethod sv-infimum (client (domain bits-used))
  (declare (ignore client))
  0) ; none used
(defmethod sv-supremum (client (domain bits-used))
  (declare (ignore client))
  -1) ; all used
(defmethod sv-subinfop (client (domain bits-used) (info1 integer) (info2 integer))
  (declare (ignore client))
  (values (zerop (logandc2 info1 info2)) t))
(defmethod sv-meet/2 (client (domain bits-used) (info1 integer) (info2 integer))
  (declare (ignore client))
  (logand info1 info2))
(defmethod sv-join/2 (client (domain bits-used) (info1 integer) (info2 integer))
  (declare (ignore client))
  (logior info1 info2))
(defmethod sv-widen (client (domain bits-used) (info1 integer) (info2 integer))
  (declare (ignore client))
  ;; If info2 isn't actually longer than info1, keep cooking.
  ;; Otherwise, try widening up to some hardcoded powers of two.
  ;; Past 64 give up, since that's usually where machine words give out.
  ;; FIXME: could be configurable. In particular we guess about fixnum bits.
  ;; TODO? For full goofiness we could work with lengths of bits instead,
  ;; regardless of whether they're anchored at the LSB. This would allow e.g.
  ;; optimizing (ldb (byte 8 128) (+ ...)) to use 8-bit arithmetic.
  (let ((i2L (integer-length info2)))
    (macrolet ((lengths (&rest n)
                 `(cond ((<= i2L (integer-length info1)) info2)
                        ((minusp info2) -1) ; dumb but valid
                        ,@(loop for i in n
                                collect `((<= i2L ,i)
                                          ;; all 1 bits of length i.
                                          ,(ldb (byte i 0) -1)))
                        (t -1))))
      (lengths 1 2 4 7 8 15 16 29 31 32 61 63 64))))
