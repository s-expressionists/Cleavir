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

;;; This is the ctype of (function * *).
(defun function-top (system)
  (function nil nil (top system) nil nil nil (values nil nil (top system) system) system))

(defun primary (values-ctype system)
  (nth-value 0 values-ctype system))

(defun single-value (non-values-ctype system)
  (values (list non-values-ctype) nil (bottom system) system))
