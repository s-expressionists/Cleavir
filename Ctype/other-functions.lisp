(in-package #:cleavir-ctype)

(defun conjoin (client &rest ctypes)
  "Return the conjunction (i.e. CL:AND) of non-values ctypes.

See VALUES-CONJOIN"
  (cond ((null ctypes) (top client))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (ct1 ct2) (conjoin/2 client ct1 ct2))
                 ctypes))))
(defun disjoin (client &rest ctypes)
  "Return the disjunction (i.e. CL:OR) of non-values ctypes.

See VALUES-DISJOIN"
  (cond ((null ctypes) (bottom client))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (ct1 ct2) (disjoin/2 client ct1 ct2))
                 ctypes))))
(defun wdisjoin (client &rest ctypes)
  (cond ((null ctypes) (bottom client))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (ct1 ct2) (wdisjoin/2 client ct1 ct2))
                 ctypes))))

(defun values-conjoin (client &rest ctypes)
  "Return the conjunction of values ctypes.

See CONJOIN"
  (cond ((null ctypes) (values-top client))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (vct1 vct2) (values-conjoin/2 client vct1 vct2))
                 ctypes))))
(defun values-disjoin (client &rest ctypes)
  "Return the disjunction ov values ctypes.

See DISJOIN"
  (cond ((null ctypes) (values-bottom client))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (vct1 vct2) (values-disjoin/2 client vct1 vct2))
                 ctypes))))
(defun values-wdisjoin (client &rest ctypes)
  (cond ((null ctypes) (values-bottom client))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (vct1 vct2) (values-wdisjoin/2 client vct1 vct2))
                 ctypes))))

(defun values-append (client &rest ctypes)
  "Append the given values ctypes together.

See VALUES-APPEND/2"
  (cond ((null ctypes) (values client nil nil (bottom client)))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (vct1 vct2) (values-append/2 client vct1 vct2))
                 ctypes))))

(defun disjointp (client ct1 ct2)
  "Return true if the two non-values ctypes are disjoint.
This is approximate: DISJOINTP may return false even if the ctypes are disjoint, if determining as much is impossible, or cannot be done efficiently.

See VALUES-DISJOINTP"
  (bottom-p client (conjoin/2 client ct1 ct2)))

;;; FIXME: Normalizing values types harder should eliminate these degenerate
;;; bottom types.
(defun values-disjointp (client vct1 vct2)
  "Return true if two values ctypes are disjoint.
This is approximate: VALUES-DISJOINTP may return false evne if the ctypes are disjoint.

See DISJOINTP"
  (let ((vcc (values-conjoin client vct1 vct2)))
    (some (lambda (ct) (bottom-p client ct)) (values-required client vcc))))

(defun function-top (client)
  "Return the ctype for CL:FUNCTION."
  (function client nil nil (top client) nil nil nil (values client nil nil (top client))))

(defun primary (client values-ctype)
  "Return the ctype for the primary value of a values ctype."
  (nth-value client 0 values-ctype))

(defun coerce-to-values (client ctype)
  "Given a non-values ctype, return a values ctype for it.
This applies fuzz, so e.g. NULL -> (VALUES &OPTIONAL NULL &REST T) while CONS -> (VALUES CONS &REST T)"
  (cond ((top-p client ctype) (values-top client))
        ((disjointp client ctype (member client nil))
         (values client (list ctype) nil (top client)))
        (t (values client nil (list ctype) (top client)))))

(defun single-value (client non-values-ctype)
  "Given a non-values ctype, return the values ctype that means exactly one value of that type."
  (values client (list non-values-ctype) nil (bottom client)))
