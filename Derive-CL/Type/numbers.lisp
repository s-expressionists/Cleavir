(in-package #:cleavir-derive-cl)

(defun type-number (client)
  (ctype:class (find-class 'number) client))

(defun range (client kind low lxp high hxp)
  (ctype:range kind
               (cond ((not low) '*) (lxp (list low)) (t low))
               (cond ((not high) '*) (hxp (list high)) (t high))
               client))

(defun range-bounds (client range)
  (multiple-value-bind (low lxp) (ctype:range-low range client)
    (multiple-value-bind (high hxp) (ctype:range-high range client)
      (values low lxp high hxp))))

(defun contagion (ty1 ty2)
  (ecase ty1
    ((integer)
     (case ty2 ((integer) ty1) ((ratio) 'rational) (t ty2)))
    ((ratio)
     (case ty2 ((integer) 'rational) ((ratio) ty1) (t ty2)))
    ((rational)
     (case ty2 ((integer ratio) ty1) (t ty2)))
    ((short-float)
     (case ty2 ((integer ratio rational short-float) ty1) (t ty2)))
    ((single-float)
     (case ty2 ((integer ratio rational short-float) ty1) (t ty2)))
    ((double-float)
     (case ty2 ((integer ratio rational short-float single-float) ty1) (t ty2)))
    ((long-float)
     (case ty2
       ((integer ratio rational short-float single-float double-float) ty1)
       (t ty2)))
    ((float)
     (case ty2
       ((integer ratio rational short-float single-float double-float long-float)
        ty1)
       (t ty2)))
    ((real) ty1)))

;; integer/integer can be a ratio, so this is contagion but lifting to rational.
(defun divcontagion (ty1 ty2)
  (let ((cont (contagion ty1 ty2)))
    (if (member cont '(integer ratio))
        'rational
        cont)))

;;; 12.1.3.3 Rule of Float Substitutability
(defun irrat-kind (kind)
  (case kind
    ((integer ratio rational) 'single-float)
    (t kind)))

(defun simple-range-2op (client op range1 range2)
  (let* ((k1 (ctype:range-kind range1 client))
         (k2 (ctype:range-kind range2 client)))
    (multiple-value-bind (low1 lxp1 high1 hxp1) (range-bounds client range1)
      (multiple-value-bind (low2 lxp2 high2 hxp2) (range-bounds client range2)
        (ctype:range (contagion k1 k2)
                     (if (or (null low1) (null low2))
                         '*
                         (let ((sum (funcall op low1 low2)))
                           (if (or lxp1 lxp2) (list sum) sum)))
                     (if (or (null high1) (null high2))
                         '*
                         (let ((sum (funcall op high1 high2)))
                           (if (or hxp1 hxp2) (list sum) sum)))
                     client)))))

(defun coerce-bound (bound kind)
  (flet ((%coerce (num)
           (ecase kind
             ((integer rational) (rational num))
             ((short-float single-float double-float long-float float)
              (coerce num kind))
             ((real) num))))
    (cond ((null bound) '*)
          ((consp bound) (list (%coerce (car bound))))
          (t (%coerce bound)))))

(defun interval->range (client kind interval)
  (ctype:range kind
               (coerce-bound (interval-low interval) kind)
               (coerce-bound (interval-high interval) kind) client))

(defun range->interval (client range)
  (multiple-value-bind (low lxp high hxp) (range-bounds client range)
    (make-interval (if lxp (list low) low) (if hxp (list high) high))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Addition and subtraction

(defun range-negate (client range)
  (multiple-value-bind (low lxp high hxp) (range-bounds client range)
    (ctype:range (ctype:range-kind range client)
                 (cond ((null high) '*)
                       (hxp (list (- high)))
                       (t (- high)))
                 (cond ((null low) '*)
                       (lxp (list (- low)))
                       (t (- low)))
                 client)))

(defun type-+ (client ty1 ty2)
  (if (and (ctype:rangep ty1 client) (ctype:rangep ty2 client))
      (simple-range-2op client #'+ ty1 ty2)
      (type-number client)))

(defun values-type-+ (client required optional rest)
  (if (or optional (not (ctype:bottom-p rest client)))
      (type-number client) ; FIXME
      (loop with result = (range client 'integer 0 nil 0 nil)
            for arg in required
            do (setf result (type-+ client result arg))
            finally (return result))))

(defun type-negate (client type)
  (distribute client
              (lambda (type)
                (if (ctype:rangep type client)
                    (range-negate client type)
                    (type-number client)))
              type))

(define-deriver (+ domain:type) (client (&rest args))
  (ctype:single-value
   (values-type-+ client
                 (ctype:values-required args client)
                 (ctype:values-optional args client)
                 (ctype:values-rest args client))
   client))

(define-deriver (- domain:type) (client (&rest args))
  (let ((required (ctype:values-required args client))
        (optional (ctype:values-optional args client))
        (rest (ctype:values-rest args client)))
    (ctype:single-value
     (cond (; FIXME
            (or optional (not (ctype:bottom-p rest client))) (type-number client))
           ((null required) (return-from - (ctype:values-bottom client)))
           ((null (rest required))
            (type-negate client (first required)))
           (t (type-+ client (first required)
                      (type-negate
                       client
                       (values-type-+ client (rest required) optional rest)))))
     client)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multiplication and integer exponentiation

(defun range-* (client range1 range2)
  (let ((i1 (range->interval client range1)) (i2 (range->interval client range2)))
    (interval->range client
                     (contagion (ctype:range-kind range1 client)
                                (ctype:range-kind range2 client))
                     (interval* i1 i2))))

(defun type-* (client type1 type2)
  (if (and (ctype:rangep type1 client) (ctype:rangep type2 client))
      (range-* client type1 type2)
      (type-number client)))

(defun range-expt (client kind low lxp high hxp exponent)
  (let ((lowe (if low (expt low exponent) low))
        (highe (if high (expt high exponent) high)))
    (cond ((oddp exponent) (range client kind lowe lxp highe hxp))
          ((not low)
           (if (and high (<= high 0))
               (range client kind highe hxp nil nil)
               (range client kind (coerce 0 kind) nil highe hxp)))
          ((not high)
           (if (and low (<= low 0))
               (range client kind (coerce 0 kind) nil highe hxp)
               (range client kind lowe lxp nil nil)))
          ((<= high 0) (range client kind highe hxp lowe lxp))
          ((< highe lowe) (range client kind highe hxp lowe lxp))
          (t (range client kind lowe lxp highe hxp)))))

(defun type-expt (client type exponent)
  ;; type is a type, exponent is an integer > 0.
  (if (= exponent 1)
      type
      (distribute client
                  (lambda (type)
                    (if (ctype:rangep type client)
                        (multiple-value-bind (low lxp high hxp)
                            (range-bounds client type)
                          (range-expt client (ctype:range-kind type client)
                                      low lxp high hxp exponent))
                        (type-number client)))
                  type)))

(define-deriver (* domain:type)
    (client (&rest args) domain:equivalence (&rest equiv))
  (when (or (not (null (ctype:values-optional args client)))
            (not (ctype:bottom-p (ctype:values-rest args client) client)))
    (return-from * (ctype:single-value (type-number client) client)))
  ;; first gather exponents for any repeated arguments.
  ;; this lets us determine for example that (* x x) is positive.
  (ctype:single-value
   (loop with equivs = ()
         with eq-sup = (domain:sv-supremum client domain:equivalence)
         for arg in (ctype:values-required args client)
         for i from 0
         for eq = (domain:info-values-nth client domain:equivalence i equiv)
         for p = (if (domain:sv-subinfop client domain:equivalence eq-sup eq)
                     nil ; info is eq-sup, so no equivalence is available
                     (assoc eq equivs))
         if p
           do (incf (third p))
         else
           do (push (list eq arg 1) equivs)
         finally (return
                   (loop with result = (range client 'integer 1 nil 1 nil)
                         for (_ range exponent) in equivs
                         for re = (type-expt client range exponent)
                         do (setf result (type-* client result re))
                         finally (return result))))
   client))

(define-deriver (expt domain:type) (client (base power))
  (ctype:single-value
   (cond ((and (ctype:rangep power client)
               (eq 'integer (ctype:range-kind power client))
               (multiple-value-bind (low lxp high hxp) (range-bounds client power)
                 (and (not lxp) (not hxp) (= low high) (> low 0))))
          ;; constant power
          (type-expt client base (ctype:range-low power client)))
         ;; otherwise we give up. TODO!
         ;; rational arguments can give complex results, so be careful
         (t (type-number client)))
   client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Division

;;; Split into two intervals, one wholly less than zero and one greater.
;;; If either range is empty, NIL is returned for it instead.
(defun range->intervals-for-reciprocal (client range)
  (multiple-value-bind (low lxp high hxp) (range-bounds client range)
    (if (eq (ctype:range-kind range client) 'integer)
        ;; Integer ranges we treat specially when they include 0,
        ;; because the interval arithmetic doesn't understand discreteness.
        ;; For example, the reciprocal of an (integer -7 7) is a rational
        ;; between -1 and 1, but the reciprocal of a
        ;; (rational -7 7) is unbounded as it approaches zero.
        ;; We also normalize exclusive bounds while we're at it.
        (values
         (if (and low (>= low (if lxp -1 0)))
             nil
             (make-interval (cond ((not low) low)
                                  (lxp (1+ low))
                                  (t low))
                            (cond ((or (not high) (>= high 0)) -1)
                                  (hxp (1- high))
                                  (t high))))
         (if (and high (<= high (if hxp 1 0)))
             nil
             (make-interval (cond ((or (not low) (<= low 0)) 1)
                                  (lxp (1+ low))
                                  (t low))
                            (cond ((not high) high)
                                  (hxp (1- high))
                                  (t high)))))
        (values
         (if (and low (>= low 0))
             nil
             (make-interval (cond ((not low) low)
                                  (lxp (list low))
                                  (t low))
                            (cond ((or (not high) (>= high 0)) '(0))
                                  (hxp (list high))
                                  (t high))))
         (if (and high (<= high 0))
             nil
             (make-interval (cond ((or (not low) (<= low 0)) '(0))
                                  (lxp (list low))
                                  (t low))
                            (cond ((not high) high)
                                  (hxp (list high))
                                  (t high))))))))

;;; Given two range types, return an interval for the result.
;;; We take types rather than intervals because the divisor being an integer
;;; can restrict the result when it crosses zero (see range->intervals-for-reciprocal)
(defun range-divide (client n1 n2)
  (let* ((int1 (range->interval client n1)))
    (multiple-value-bind (below2 above2)
        (range->intervals-for-reciprocal client n2)
      (let ((rbelow (if below2
                        (interval-negate
                         (interval*-1-pos
                          (interval-reciprocal-+ (interval-negate below2))
                          int1))
                        nil))
            (rabove (if above2
                        (interval*-1-pos (interval-reciprocal-+ above2) int1)
                        nil)))
        (cond ((and rbelow rabove) (interval-merge rbelow rabove))
              (rbelow rbelow)
              (rabove rabove)
              ;; arises from division by zero.
              ;; this can result in infinities and NaN, so we punt a bit.
              ;; FIXME: We could be a bit more intelligent, e.g. NIL type
              ;; for rationals, and get the sign of infinities.
              (t (make-unbounded-interval)))))))

(defun range-reciprocal (client type)
  (let* ((kind (ctype:range-kind type client))
         (rkind (if (eq kind 'integer) 'rational kind)))
    (multiple-value-bind (below above)
        (range->intervals-for-reciprocal client type)
      (let ((rbelow
              (if below
                  (interval-negate
                   (interval-reciprocal-+ (interval-negate below)))
                  nil))
            (rabove
              (if above
                  (interval-reciprocal-+ above)
                  nil)))
        (cond (rabove
               (interval->range
                client rkind (if rbelow
                                 (interval-merge rbelow rabove)
                                 rabove)))
              (rbelow (interval->range rbelow rkind client))
              ;; Both being NIL happens if the input range is all zero.
              ;; As in / above, this can result in infinities rather than
              ;; being an error, so we punt.
              (t (ctype:range rkind '* '* client)))))))

(defun type-reciprocal (client type)
  (distribute
   client
   (lambda (type)
     (if (ctype:rangep type client)
         (range-reciprocal client type)
         (type-number client)))
   type))

(defun range-/ (client range1 range2)
  (interval->range client (divcontagion (ctype:range-kind range1 client)
                                        (ctype:range-kind range2 client))
                   (range-divide client range1 range2)))

(defun type-/ (client type1 type2)
  (if (and (ctype:rangep type1 client) (ctype:rangep type2 client))
      (range-/ client type1 type2)
      (type-number client)))

(define-deriver (/ domain:type) (client (&rest args))
  (let ((required (ctype:values-required args client))
        (optional (ctype:values-optional args client))
        (rest (ctype:values-rest args client)))
    (ctype:single-value
     (cond (; FIXME
            (or optional (not (ctype:bottom-p rest client))) (type-number client))
           ((null required) (return-from / (ctype:values-bottom client)))
           ((null (rest required)) (type-reciprocal client (first required)))
           (t (type-/ client (first required)
                      ;; obviously this is what the * deriver does, but
                      ;; without equivalence information. Could soup that up if
                      ;; we really want to. TODO?
                      (loop with result = (range client 'integer 1 nil 1 nil)
                            for re in (rest required)
                            do (setf result (type-* client result re))
                            finally (return result)))))
     client)))

(defun derive-floor-etc (client dividend divisor quokindfun quofun remfun)
  (if (and (ctype:rangep dividend client) (ctype:rangep divisor client))
      ;; The CLHS actually only says that the remainder
      ;; is a float if an argument is a float, i.e. it doesn't
      ;; specify that it has to be a double given doubles, etc.
      ;; Instead we use the usual contagion rules for the remainder,
      ;; as per WSCL issue FLOOR-ETC-REMAINDER-TYPE. If an implementation
      ;; does something else it can just not use these derivers.
      (let* ((dividend-kind (ctype:range-kind dividend client))
             (divisor-kind (ctype:range-kind divisor client))
             (rkind (contagion dividend-kind divisor-kind)))
        (ctype:values
         (list (interval->range
                client (funcall quokindfun dividend-kind divisor-kind)
                (funcall quofun (range-divide client dividend divisor)))
               (interval->range
                client rkind
                (funcall remfun (range->interval client dividend)
                         (range->interval client divisor))))
         nil (ctype:bottom client) client))
      (ctype:values (list (ctype:range (funcall quokindfun 'real 'real)
                                       '* '* client)
                          (ctype:range 'real '* '* client))
                    nil (ctype:bottom client) client)))

(defun floor-quokind (k1 k2) (declare (ignore k1 k2)) 'integer)

(define-deriver (truncate domain:type)
    (client (dividend &optional (divisor (range client 'integer 1 nil 1 nil))))
  (derive-floor-etc client dividend divisor
                    #'floor-quokind #'interval-truncate #'truncate-remainder))
(define-deriver (floor domain:type)
    (client (dividend &optional (divisor (range client 'integer 1 nil 1 nil))))
  (derive-floor-etc client dividend divisor
                    #'floor-quokind #'interval-floor #'floor-remainder))
(define-deriver (ceiling domain:type)
    (client (dividend &optional (divisor (range client 'integer 1 nil 1 nil))))
  (derive-floor-etc client dividend divisor
                    #'floor-quokind #'interval-ceiling #'ceiling-remainder))

(define-deriver (mod domain:type) (client (number divisor))
  (ctype:single-value
   (if (and (ctype:rangep number client) (ctype:rangep divisor client))
       (interval->range client
                        (contagion (ctype:range-kind number client)
                                   (ctype:range-kind divisor client))
                        (floor-remainder (range->interval client number)
                                         (range->interval client divisor)))
       (range client 'real nil nil nil nil))
   client))
(define-deriver (rem domain:type) (client (number divisor))
  (ctype:single-value
   (if (and (ctype:rangep number client) (ctype:rangep divisor client))
       (interval->range client
                        (contagion (ctype:range-kind number client)
                                   (ctype:range-kind divisor client))
                        (truncate-remainder (range->interval client number)
                                            (range->interval client divisor)))
       (range client 'real nil nil nil nil))
   client))

;;; The specification of the quotient's type in the CLHS is self-contradictory:
;;; Arguments and Types says they return a float, and this is presumably the point
;;; of the functions (as opposed to floor et al. which return integer quotients),
;;; but the description says the quotient type is mostly determined by the usual
;;; contagion rules, which would mean e.g. (ffloor 2 3) should return a rational.
;;; Instead we do the following: If both arguments are rational, a single float.
;;; Otherwise, a float of the largest format among the arguments.
(defun ffloor-quokind (k1 k2)
  (cond ((or (member k1 '(float real)) (member k2 '(float real))) 'float)
        ((and (member k1 '(integer ratio real)) (member k2 '(integer ratio real)))
         'single-float)
        (t (contagion k1 k2))))

(define-deriver (ffloor domain:type)
    (client (dividend &optional (divisor (range client 'integer 1 nil 1 nil))))
  (derive-floor-etc client dividend divisor
                    #'ffloor-quokind #'interval-floor #'floor-remainder))
(define-deriver (fceiling domain:type)
    (client (dividend &optional (divisor (range client 'integer 1 nil 1 nil))))
  (derive-floor-etc client dividend divisor
                    #'ffloor-quokind #'interval-ceiling #'ceiling-remainder))
(define-deriver (ftruncate domain:type)
    (client (dividend &optional (divisor (range client 'integer 1 nil 1 nil))))
  (derive-floor-etc client dividend divisor
                    #'ffloor-quokind #'interval-truncate #'truncate-remainder))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Trigonometry and other irrational operations

;;; Given an irrational monotonic function, and a range for its one argument,
;;; return a range for the result. Assumes that the function returns an irrational
;;; (i.e. a float) even in the few cases that the function may not be irrational,
;;; e.g. (sin 0) => 0.0
(defun range-irrat-monotonic1 (client range function
                               &key (inf '*) (sup '*) decreasing)
  (let* ((kind (ctype:range-kind range client))
         (mkind (irrat-kind kind)))
    (multiple-value-bind (low lxp high hxp) (range-bounds client range)
      (let ((olow (cond ((not low)
                         (if (numberp inf) (coerce inf mkind) inf))
                        (lxp (list (funcall function low)))
                        (t (funcall function low))))
            (ohigh (cond ((not high)
                          (if (numberp sup) (coerce sup mkind) sup))
                         (hxp (list (funcall function high)))
                         (t (funcall function high)))))
        (if decreasing
            (ctype:range mkind ohigh olow client)
            (ctype:range mkind olow ohigh client))))))

(defun type-irrat-monotonic1 (client type function &key (inf '*) (sup '*))
  (distribute client (lambda (ty)
                       (if (ctype:rangep ty client)
                           (range-irrat-monotonic1 client ty function
                                                   :inf inf :sup sup)
                           (type-number client)))
              type))

(defun range-bound-irrat-monotonic1 (client range function lowb highb
                                     &key (inf '*) (sup '*) decreasing)
  (let ((low (ctype:range-low range client))
        (high (ctype:range-high range client)))
    (if (and low (>= low lowb)
             high (<= high highb))
        (range-irrat-monotonic1 client range function :inf inf :sup sup
                                :decreasing decreasing)
        (type-number client))))

(defun type-bound-irrat-monotonic1 (client type function lowb highb
                                    &key (inf '*) (sup '*) decreasing)
  (distribute client (lambda (ty)
                       (range-bound-irrat-monotonic1 client ty function lowb highb
                                                     :inf inf :sup sup
                                                     :decreasing decreasing))
              type))

(defun range-boundbelow-irrat-monotonic1 (client range function lowbound
                                          &key (inf '*) (sup '*))
  (let ((low (ctype:range-low range client)))
    (if (and low (>= low lowbound))
        (range-irrat-monotonic1 client range function :inf inf :sup sup)
        (type-number client))))

(defun type-boundbelow-irrat-monotonic1 (client type function lowbound
                                         &key (inf '*) (sup '*))
  (distribute client (lambda (ty)
                       (range-boundbelow-irrat-monotonic1 client ty function
                                                          lowbound :inf inf :sup sup))
              type))

;;; Does the interval contain offset + n*multiple for some integer n?
(defun interval-contains-periodically (low lxp high hxp offset multiple)
  (when (or (not low) (not high))
    (return-from interval-contains-periodically t))
  (let ((lonext (* multiple (ceiling (+ low offset) multiple)))
        (hioff (+ high offset)))
    (when (= lonext low) ; low happens to be right on a boundary
      (unless lxp (return-from interval-contains-periodically t))
      (incf lonext multiple))
    (or (> hioff lonext) (and hxp (= hioff lonext)))))

(defun range-sincos (client range offset)
  (multiple-value-bind (low lxp high hxp) (range-bounds client range)
    (let ((kind (irrat-kind (ctype:range-kind range client))))
      (when (or (null low) (null high))
        (return-from range-sincos
          (ctype:range kind (coerce -1 kind) (coerce -1 kind) client)))
      (let ((sl (sin (+ low (coerce offset kind)))) (sh (sin (+ high (coerce offset kind)))))
        (multiple-value-bind (rlow rlxp)
            (cond ((interval-contains-periodically low lxp high hxp
                                                   (+ offset (/ pi 2)) (* 2 pi))
                   ;; contains a minimum
                   (values (coerce -1 kind) nil))
                  ((< sl sh) (values sl lxp))
                  (t (values sh hxp)))
          (multiple-value-bind (rhigh rhxp)
              (cond ((interval-contains-periodically low lxp high hxp
                                                     (- offset (/ pi 2)) (* 2 pi))
                     ;; contains a maximum
                     (values (coerce 1 kind) nil))
                    ((> sl sh) (values sl lxp))
                    (t (values sh hxp)))
            (range client kind rlow rlxp rhigh rhxp)))))))

(define-deriver (exp domain:type) (client (arg))
  (ctype:single-value (type-irrat-monotonic1 client arg #'exp :inf 0f0) client))

(define-deriver (sqrt domain:type) (client (arg))
  (type-boundbelow-irrat-monotonic1 client arg #'sqrt 0 :inf 0f0))

(define-deriver (sin domain:type) (client (arg))
  (ctype:single-value
   (distribute client (lambda (ty) (if (ctype:rangep ty client)
                                       (range-sincos client ty 0)
                                       (type-number client)))
               arg)
   client))
(define-deriver (cos domain:type) (client (arg))
  (ctype:single-value
   (distribute client (lambda (ty) (if (ctype:rangep ty client)
                                       (range-sincos client ty (/ pi 2))
                                       (type-number client)))
               arg)
   client))
(define-deriver (tan domain:type) (client (arg))
  (ctype:single-value
   (distribute
    client (lambda (ty)
             (if (ctype:rangep ty client)
                 (multiple-value-bind (low lxp high hxp) (range-bounds client ty)
                   (multiple-value-bind (rlow rlxp rhigh rhxp)
                       (if (interval-contains-periodically low lxp high hxp
                                                           (/ pi 2) pi)
                           ;; contains an asymptote
                           (values '* nil '* nil)
                           ;; doesn't, so we're monotonic
                           (values (tan low) lxp (tan high) hxp))
                     (range client (irrat-kind (ctype:range-kind ty client))
                            rlow rlxp rhigh rhxp)))
                 (type-number client)))
    arg)
   client))

(define-deriver (asin domain:type) (client (arg))
  (ctype:single-value
   (type-bound-irrat-monotonic1 client arg #'asin -1 1
                                :inf (- (/ pi 2)) :sup (/ pi 2))
   client))
(define-deriver (acos domain:type) (client (arg))
  (ctype:single-value
   (type-bound-irrat-monotonic1 client arg #'acos -1 1 :inf 0 :sup pi :decreasing t)
   client))

(define-deriver (sinh domain:type) (client (arg))
  (ctype:single-value (type-irrat-monotonic1 client arg #'sinh) client))
(define-deriver (cosh domain:type) (client (arg))
  (ctype:single-value
   (let ((kind (irrat-kind (ctype:range-kind arg client))))
     (multiple-value-bind (low lxp high hxp) (range-bounds client arg)
       (multiple-value-bind (rlow rlxp)
           (cond ((not low)
                  (if (not high)
                      (values (coerce 1 kind) nil)
                      (values (cosh high) hxp)))
                 ((zerop low) (values (coerce 1 kind) nil))
                 ((< low 0)
                  (if (or (not high) (>= high 0))
                      (values (coerce 1 kind) nil)
                      ;; both low and high are negative
                      (values (cosh high) hxp))))
         (multiple-value-bind (rhigh rhxp)
             (cond ((not low)
                    (if (not high)
                        (values '* nil)
                        (values (cosh high) hxp)))
                   ((not high) (values (cosh low) lxp))
                   (t (let ((sl (cosh low)) (sh (cosh high)))
                        (if (> sl sh)
                            (values sh hxp)
                            (values sl lxp)))))
           (range client kind rlow rlxp rhigh rhxp)))))
   client))
(define-deriver (tanh domain:type) (client (arg))
  (ctype:single-value (type-irrat-monotonic1 client arg #'tanh :inf -1f0 :sup 1f0)
                      client))

(define-deriver (abs domain:type) (client (arg))
  (ctype:single-value
   (distribute
    client
    (lambda (type)
      (if (ctype:rangep type client)
          (let ((kind (ctype:range-kind arg client)))
            (multiple-value-bind (low lxp high hxp) (range-bounds client arg)
              (ctype:range kind
                           (cond ((or (not low) (and low (minusp low)))
                                  (coerce 0 kind))
                                 ((or (not high) (< low (abs high)))
                                  (if lxp (list low) low))
                                 (t (if hxp (list (abs high)) (abs high))))
                           (cond ((or (not high) (not low)) '*)
                                 ((< (abs low) (abs high))
                                  (if hxp (list (abs high)) (abs high)))
                                 (t (if lxp (list (abs low)) (abs low))))
                           client)))
          (ctype:range 'real 0 '* client)))
    arg)
   client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bitwise operations

;;; Return (values low high) for the given range. high can be *.
(defun %range-integer-length (low high)
  ;; We compute bounds based on integer-length being nondecreasing
  ;; from 0 on up and from -1 on down. So if we're entirely positive
  ;; or negative we just work monotonically, otherwise min is zero
  ;; and max is whatever's biggest.
  (cond ((and low (> low 0))
         ;; entirely positive range.
         (values (integer-length low)
                 (if high (integer-length high) '*)))
        ((and high (< high 0))
         ;; entirely negative
         (values (integer-length high)
                 (if low (integer-length low) '*)))
        (t
         ;; zero-crossing
         (values 0 (if (and low high)
                       (max (integer-length low) (integer-length high))
                       '*)))))

(defun range-integer-length (client type)
  (multiple-value-bind (low high emptyp) (range-integer-bounds client type)
    (if emptyp
        (ctype:bottom client)
        (multiple-value-call #'ctype:range
          'integer (%range-integer-length low high) client))))

(defun type-integer-length (client type)
  (distribute client
              (lambda (ty)
                (if (ctype:rangep ty client)
                    (range-integer-length client ty)
                    (ctype:range 'integer 0 '* client)))
              type))

(define-deriver (integer-length domain:type) (client (arg))
  (ctype:single-value (type-integer-length client arg) client))

;;; LOGNOT in Lisp's conception is monotonic decreasing.
(defun range-lognot (low high)
  (values (if high (lognot high) nil)
          (if low (lognot low) nil)))
(define-deriver (lognot domain:type) (client (arg))
  (ctype:single-value
   (distribute client
               (lambda (ty)
                 (cond ((not (ctype:rangep ty client))
                        (ctype:range 'integer '* '* client))
                       ((not (member (ctype:range-kind ty client)
                                     '(integer rational real)))
                        ;; we're passing a float or something, which is invalid
                        (ctype:bottom client))
                       (t
                        (multiple-value-bind (low high emptyp)
                            (range-integer-bounds client ty)
                          (if emptyp
                              (ctype:bottom client)
                              (ctype:range 'integer
                                           (if high (lognot high) '*)
                                           (if low (lognot low) '*)
                                           client))))))
               arg)
   client))

;;; Getting better bounds for these bitwise operations is nontrivial.
;;; However we can use the following fairly elementary facts:
;;; (<= (integer-length (logand x y)) (max (integer-length x) (integer-length y)))
;;; (<= (integer-length (logior x y)) (max (integer-length x) (integer-length y)))
;;; (<= (integer-length (logxor x y)) (max (integer-length x) (integer-length y)))
;;; (= (integer-length (lognot x)) (integer-length x))
;;; In other words these operations can't add bits. This is enough for a lot of
;;; optimization, in that e.g. given fixnums a fixnum comes out.
;;; If the arguments are nonnegative, the max for logand can be replaced with min,
;;; and ditto for logior if they're all nonpositive.
;;; If some argument to LOGAND is nonnegative the result is too, and ditto for
;;; LOGIOR and nonpositive.
(defun range-logand/2 (low1 high1 low2 high2)
  (let ((nn1 (and low1 (>= low1 0))) (nn2 (and low2 (>= low2 0))))
    (cond ((and nn1 nn2)
           (values 0 (if (or (not high1) (not high2))
                         nil
                         (1- (ash 1 (min (integer-length high1) (integer-length high2)))))))
          ((or nn1 nn2)
           (values 0 (if (or (not high1) (not high2))
                         nil
                         (1- (ash 1 (max (integer-length high1) (integer-length high2)))))))
          ((and low1 high1 low2 high2)
           (let ((bits (max (integer-length low1) (integer-length low2)
                            (integer-length high1) (integer-length high2))))
             (values (- (ash 1 bits)) (1- (ash 1 bits)))))
          (t (values nil nil)))))
(defun range-logior/2 (low1 high1 low2 high2)
  (let ((np1 (and high1 (<= high1 0))) (np2 (and high2 (<= high2 0))))
    (cond ((and np1 np2)
           (values (if (or (not low1) (not low2))
                       nil
                       (- (ash 1 (min (integer-length low1) (integer-length low2)))))
                   0))
          ((or np1 np2)
           (values (if (or (not low1) (not low2))
                       nil
                       (- (ash 1 (max (integer-length low1) (integer-length low2)))))
                   0))
          ((and low1 high1 low2 high2)
           (let ((bits (max (integer-length low1) (integer-length low2)
                            (integer-length high1) (integer-length high2))))
             (values (- (ash 1 bits)) (1- (ash 1 bits)))))
          (t (values nil nil)))))
(defun range-logxor/2 (low1 high1 low2 high2)
  ;; for logxor, the result is negative iff exactly one argument is negative.
  (let ((nn1 (and low1 (>= low1 0))) (np1 (and high1 (<= high1 0)))
        (nn2 (and low2 (>= low2 0))) (np2 (and high2 (<= high2 0)))
        (nbits (if (and low1 high1 low2 high2)
                   (max (integer-length low1) (integer-length high1)
                        (integer-length low2) (integer-length high2))
                   nil)))
    (cond ((and nn1 np2) ; +-
           (values (if nbits (- (ash 1 nbits)) nil) 0))
          ((and np1 nn2) ; -+
           (values (if nbits (- (ash 1 nbits)) nil) 0))
          ((and nn1 nn2) ; ++
           (values 0 (if nbits (1- (ash 1 nbits)) nil)))
          ((and np1 np2) ; --
           (values 0 (if nbits (1- (ash 1 nbits)) nil)))
          (nbits
           (values (- (ash 1 nbits)) (1- (ash 1 nbits))))
          (t (values nil nil)))))

(define-deriver (logand domain:type) (client (&rest args))
  ;; FIXME
  (when (or (not (null (ctype:values-optional args client)))
            (not (ctype:bottom-p (ctype:values-rest args client) client)))
    (return-from logand (ctype:single-value (type-number client) client)))
  (ctype:single-value
   (flet ((min-bits (a b) (cond ((not a) b) ((not b) a) (t (min a b))))
          (max-bits (a b) (cond ((not a) a) ((not b) b) (t (max a b)))))
     (loop with min-nbits = nil with max-nbits = 0
           with some-nonnegativep = nil
           with all-nonnegativep = t
           for arg in (ctype:values-required args client)
           do (multiple-value-bind (low high emptyp) (type-integer-bounds client arg)
                (when emptyp (return (ctype:bottom client))) ; strictness
                (if (and low (>= low 0))
                    (setf some-nonnegativep t)
                    (setf all-nonnegativep nil))
                (let* ((lowbits (if low (integer-length low) nil))
                       (highbits (if high (integer-length high) nil))
                       (max (max-bits lowbits highbits)))
                  (setf min-nbits (min-bits min-nbits max)
                        max-nbits (max-bits max-nbits max))))
           finally (return
                     (cond (all-nonnegativep
                            (ctype:range 'integer 0 (1- (ash 1 min-nbits)) client))
                           (some-nonnegativep
                            (ctype:range 'integer 0 (if max-nbits
                                                        (1- (ash 1 max-nbits))
                                                        '*)
                                         client))
                           (t
                            (if max-nbits
                                (ctype:range 'integer
                                             (- (ash 1 max-nbits))
                                             (1- (ash 1 max-nbits))
                                             client)
                                (ctype:range 'integer '* '* client)))))))
   client))
(define-deriver (logior domain:type) (client (&rest args))
  ;; FIXME
  (when (or (not (null (ctype:values-optional args client)))
            (not (ctype:bottom-p (ctype:values-rest args client) client)))
    (return-from logior (ctype:single-value (type-number client) client)))
  (ctype:single-value
   (flet ((min-bits (a b) (cond ((not a) b) ((not b) a) (t (min a b))))
          (max-bits (a b) (cond ((not a) a) ((not b) b) (t (max a b)))))
     (loop with min-nbits = nil with max-nbits = 0
           with some-nonpositivep = nil
           with all-nonpositivep = t
           for arg in (ctype:values-required args client)
           do (multiple-value-bind (low high emptyp) (type-integer-bounds client arg)
                (when emptyp (return (ctype:bottom client)))
                (if (and high (<= high 0))
                    (setf some-nonpositivep t)
                    (setf all-nonpositivep nil))
                (let* ((lowbits (if low (integer-length low) nil))
                       (highbits (if high (integer-length high) nil))
                       (max (max-bits lowbits highbits)))
                  (setf min-nbits (min-bits min-nbits max)
                        max-nbits (max-bits max-nbits max))))
           finally (return
                     (cond (all-nonpositivep
                            (ctype:range 'integer (- (ash 1 min-nbits)) 0 client))
                           (some-nonpositivep
                            (ctype:range 'integer (if max-nbits
                                                      (- (ash 1 max-nbits))
                                                      '*)
                                         0 client))
                           (t
                            (if max-nbits
                                (ctype:range 'integer
                                             (- (ash 1 max-nbits))
                                             (1- (ash 1 max-nbits))
                                             client)
                                (ctype:range 'integer '* '* client)))))))
   client))

(define-deriver (logxor domain:type) (client (&rest arguments))
  (ctype:single-value
   (multiple-value-bind (low high emptyp)
       (let ((required (ctype:values-required client arguments))
             (optional (ctype:values-optional client arguments))
             (rest (ctype:values-rest client arguments)))
         (if (and (null optional) (ctype:bottom-p rest client))
             ;; fixed number of arguments: reduce range-logxor/2
             (loop with emptyp = t with min with max
                   for type in required
                   do (multiple-value-bind (low high subemptyp)
                          (type-integer-bounds client type)
                        (when subemptyp (return (values nil nil t)))
                        (if emptyp
                            (setf emptyp nil min low max high)
                            (multiple-value-bind (low high subemptyp)
                                (range-logxor/2 min max low high)
                              (when subemptyp (return (values nil nil t)))
                              (setf min low max high)))))
             ;; variable arguments, so default to maximizing integer-length
             ;; we could also do the sign thing to some extent but it's annoying:
             ;; e.g. (&rest (integer * 0)) is negative iff there are an odd number
             ;; of arguments.
             (loop with emptyp = nil with nbits = 0
                   for type in (rest-types client arguments)
                   do (multiple-value-bind (low high subemptyp)
                          (type-integer-bounds client type)
                        (when subemptyp (return (values nil nil t)))
                        (cond ((not nbits))
                              ((or (not low) (not high)) (setf nbits nil))
                              (t (setf nbits (max nbits
                                                  (integer-length low)
                                                  (integer-length high))))))
                   finally (return
                             (values (- (ash 1 nbits)) (1- (ash 1 nbits)) nil)))))
     (if emptyp
         (ctype:bottom client)
         (ctype:range 'integer low high client)))
   client))

;;; like logxor, but with an extra lognot at each step
(define-deriver (logeqv domain:type) (client (&rest arguments))
  (ctype:single-value
   (multiple-value-bind (low high emptyp)
       (let ((required (ctype:values-required client arguments))
             (optional (ctype:values-optional client arguments))
             (rest (ctype:values-rest client arguments)))
         (if (and (null optional) (ctype:bottom-p rest client))
             ;; fixed number of arguments: reduce range-logxor/2
             (loop with emptyp = t with min with max
                   for type in required
                   do (multiple-value-bind (low high subemptyp)
                          (type-integer-bounds client type)
                        (when subemptyp (return (values nil nil t)))
                        (if emptyp
                            (setf emptyp nil min low max high)
                            (multiple-value-bind (low high subemptyp)
                                (range-logxor/2 min max low high)
                              (when subemptyp (return (values nil nil t)))
                              (multiple-value-bind (low high) (range-lognot low high)
                                (setf min low max high))))))
             ;; we're just doing length with variable arguments,
             ;; so don't bother with lognot (which preserves length)
             (loop with emptyp = nil with nbits = 0
                   for type in (rest-types client arguments)
                   do (multiple-value-bind (low high subemptyp)
                          (type-integer-bounds client type)
                        (when subemptyp (return (values nil nil t)))
                        (cond ((not nbits))
                              ((or (not low) (not high)) (setf nbits nil))
                              (t (setf nbits (max nbits
                                                  (integer-length low)
                                                  (integer-length high))))))
                   finally (return
                             (values (- (ash 1 nbits)) (1- (ash 1 nbits)) nil)))))
     (if emptyp
         (ctype:bottom client)
         (ctype:range 'integer low high client)))
   client))

(define-deriver (logandc1 domain:type) (client (a1 a2))
  (ctype:single-value
   (block nil
     (multiple-value-bind (plow1 phigh1 emptyp) (type-integer-bounds client a1)
       (when emptyp (return (ctype:bottom client)))
       (multiple-value-bind (low1 high1) (range-lognot plow1 phigh1)
         (multiple-value-bind (low2 high2 emptyp) (type-integer-bounds client a2)
           (when emptyp (return (ctype:bottom client)))
           (multiple-value-bind (low high) (range-logand/2 low1 high1 low2 high2)
             (ctype:range 'integer low high client))))))
   client))

(define-deriver (logandc2 domain:type) (client (a1 a2))
  (ctype:single-value
   (block nil
     (multiple-value-bind (low1 high1 emptyp) (type-integer-bounds client a1)
       (when emptyp (return (ctype:bottom client)))
       (multiple-value-bind (plow2 phigh2 emptyp) (type-integer-bounds client a2)
         (when emptyp (return (ctype:bottom client)))
         (multiple-value-bind (low2 high2) (range-lognot plow2 phigh2)
           (multiple-value-bind (low high) (range-logand/2 low1 high1 low2 high2)
             (ctype:range 'integer low high client))))))
   client))

(define-deriver (lognand domain:type) (client (a1 a2))
  (ctype:single-value
   (block nil
     (multiple-value-bind (low1 high1 emptyp) (type-integer-bounds client a1)
       (when emptyp (return (ctype:bottom client)))
       (multiple-value-bind (low2 high2 emptyp) (type-integer-bounds client a2)
         (when emptyp (return (ctype:bottom client)))
         (multiple-value-bind (plow phigh) (range-logand/2 low1 high1 low2 high2)
           (multiple-value-bind (low high) (range-lognot plow phigh)
             (ctype:range 'integer low high client))))))
   client))

(define-deriver (logorc1 domain:type) (client (a1 a2))
  (ctype:single-value
   (block nil
     (multiple-value-bind (plow1 phigh1 emptyp) (type-integer-bounds client a1)
       (when emptyp (return (ctype:bottom client)))
       (multiple-value-bind (low1 high1) (range-lognot plow1 phigh1)
         (multiple-value-bind (low2 high2 emptyp) (type-integer-bounds client a2)
           (when emptyp (return (ctype:bottom client)))
           (multiple-value-bind (low high) (range-logior/2 low1 high1 low2 high2)
             (ctype:range 'integer low high client))))))
   client))

(define-deriver (lognor domain:type) (client (a1 a2))
  (ctype:single-value
   (block nil
     (multiple-value-bind (low1 high1 emptyp) (type-integer-bounds client a1)
       (when emptyp (return (ctype:bottom client)))
       (multiple-value-bind (low2 high2 emptyp) (type-integer-bounds client a2)
         (when emptyp (return (ctype:bottom client)))
         (multiple-value-bind (plow phigh) (range-logior/2 low1 high1 low2 high2)
           (multiple-value-bind (low high) (range-lognot plow phigh)
             (ctype:range 'integer low high client))))))
   client))
