(in-package #:cleavir-ctype)

(defun conjoin (system &rest ctypes)
  "Return the conjunction (i.e. CL:AND) of non-values ctypes.

See VALUES-CONJOIN"
  (cond ((null ctypes) (top system))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (ct1 ct2) (conjoin/2 ct1 ct2 system))
                 ctypes))))
(defun disjoin (system &rest ctypes)
  "Return the disjunction (i.e. CL:OR) of non-values ctypes.

See VALUES-DISJOIN"
  (cond ((null ctypes) (bottom system))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (ct1 ct2) (disjoin/2 ct1 ct2 system))
                 ctypes))))
(defun wdisjoin (system &rest ctypes)
  (cond ((null ctypes) (bottom system))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (ct1 ct2) (wdisjoin/2 ct1 ct2 system))
                 ctypes))))

(defun values-conjoin (system &rest ctypes)
  "Return the conjunction of values ctypes.

See CONJOIN"
  (cond ((null ctypes) (values-top system))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (vct1 vct2) (values-conjoin/2 vct1 vct2 system))
                 ctypes))))
(defun values-disjoin (system &rest ctypes)
  "Return the disjunction ov values ctypes.

See DISJOIN"
  (cond ((null ctypes) (values-bottom system))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (vct1 vct2) (values-disjoin/2 vct1 vct2 system))
                 ctypes))))
(defun values-wdisjoin (system &rest ctypes)
  (cond ((null ctypes) (values-bottom system))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (vct1 vct2) (values-wdisjoin/2 vct1 vct2 system))
                 ctypes))))

(defun values-append (system &rest ctypes)
  "Append the given values ctypes together.

See VALUES-APPEND/2"
  (cond ((null ctypes) (values nil nil (bottom system) system))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (vct1 vct2) (values-append/2 vct1 vct2 system))
                 ctypes))))

(defun disjointp (ct1 ct2 system)
  "Return true if the two non-values ctypes are disjoint.
This is approximate: DISJOINTP may return false even if the ctypes are disjoint, if determining as much is impossible, or cannot be done efficiently.

See VALUES-DISJOINTP"
  (bottom-p (conjoin/2 ct1 ct2 system) system))

;;; FIXME: Normalizing values types harder should eliminate these degenerate
;;; bottom types.
(defun values-disjointp (vct1 vct2 system)
  "Return true if two values ctypes are disjoint.
This is approximate: VALUES-DISJOINTP may return false evne if the ctypes are disjoint.

See DISJOINTP"
  (let ((vcc (values-conjoin system vct1 vct2)))
    (some (lambda (ct) (bottom-p ct system)) (values-required vcc system))))

(defun function-top (system)
  "Return the ctype for CL:FUNCTION."
  (function nil nil (top system) nil nil nil (values nil nil (top system) system) system))

(defun primary (values-ctype system)
  "Return the ctype for the primary value of a values ctype."
  (nth-value 0 values-ctype system))

(defun coerce-to-values (ctype sys)
  "Given a non-values ctype, return a values ctype for it.
This applies fuzz, so e.g. NULL -> (VALUES &OPTIONAL NULL &REST T) while CONS -> (VALUES CONS &REST T)"
  (cond ((top-p ctype sys) (values-top sys))
        ((disjointp ctype (member sys nil) sys)
         (values (list ctype) nil (top sys) sys))
        (t (values nil (list ctype) (top sys) sys))))

(defun single-value (non-values-ctype system)
  "Given a non-values ctype, return the values ctype that means exactly one value of that type."
  (values (list non-values-ctype) nil (bottom system) system))
