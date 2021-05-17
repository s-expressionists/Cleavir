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

(defun disjointp (ct1 ct2 system)
  (bottom-p (conjoin/2 ct1 ct2 system) system))

;;; FIXME: Normalizing values types harder should eliminate these degenerate
;;; bottom types.
(defun values-disjointp (vct1 vct2 system)
  (let* ((vcc (values-conjoin vct1 vct2 system))
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

(defun single-value (non-values-ctype system)
  (values (list non-values-ctype) nil (bottom system) system))
