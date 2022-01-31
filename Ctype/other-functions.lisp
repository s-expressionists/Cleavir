(in-package #:cleavir-ctype)

(defun conjoin (system &rest ctypes)
  (cond ((null ctypes) (top system))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (ct1 ct2) (conjoin/2 ct1 ct2 system))
                 ctypes))))
(defun disjoin (system &rest ctypes)
  (cond ((null ctypes) (bottom system))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (ct1 ct2) (disjoin/2 ct1 ct2 system))
                 ctypes))))

(defun values-conjoin (system &rest ctypes)
  (cond ((null ctypes) (values-top system))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (vct1 vct2) (values-conjoin/2 vct1 vct2 system))
                 ctypes))))
(defun values-disjoin (system &rest ctypes)
  (cond ((null ctypes) (values-bottom system))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (vct1 vct2) (values-disjoin/2 vct1 vct2 system))
                 ctypes))))

(defun values-append (system &rest ctypes)
  (cond ((null ctypes) (values-bottom system))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (vct1 vct2) (values-append/2 vct1 vct2 system))
                 ctypes))))

(defun disjointp (ct1 ct2 system)
  (bottom-p (conjoin/2 ct1 ct2 system) system))

;;; FIXME: Normalizing values types harder should eliminate these degenerate
;;; bottom types.
(defun values-disjointp (vct1 vct2 system)
  (let* ((vcc (values-conjoin system vct1 vct2))
         (required (values-required vcc system)))
    (flet ((botp (ct) (bottom-p ct system)))
      (or (some #'botp required)
          (and (null required) (every #'botp (values-optional vcc system))
               (botp (values-rest vcc system)))))))

;;; This is the ctype of (function * *).
(defun function-top (system)
  (function nil nil (top system) nil nil nil (values nil nil (top system) system) system))

(defun primary (values-ctype system)
  (nth-value 0 values-ctype system))

;;; Given a non-values ctype, return a values ctype for it.
;;; This applies fuzz, so e.g. NULL -> (VALUES &OPTIONAL NULL &REST T)
;;; while CONS -> (VALUES CONS &REST T)
(defun coerce-to-values (ctype sys)
  (cond ((top-p ctype sys) (values-top sys))
        ((disjointp ctype (member sys nil) sys)
         (values (list ctype) nil (top sys) sys))
        (t (values nil (list ctype) (top sys) sys))))

(defun single-value (non-values-ctype system)
  (values (list non-values-ctype) nil (bottom system) system))
