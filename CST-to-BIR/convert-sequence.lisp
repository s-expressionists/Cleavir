(in-package #:cleavir-cst-to-bir)

;;; Utility functions for converting a sequences CSTs, represented
;;; as chains of CONS-CSTs terminated by NIL CSTs.

;;; Convert a CST representing a sequence of forms for effect, i.e.
;;; discarding values. Returns false iff the forms always exit
;;; abnormally.
(defun convert-sequence-for-effect (sequence-cst inserter env system)
  (loop with rv
        for cst = sequence-cst then (cst:rest cst)
        until (cst:null cst)
        do (setf rv (convert (cst:first cst) inserter env system))
        when (eq rv :no-return)
          return nil
        finally (return t)))

;;; As the above, but takes a Common Lisp list of CSTs instead.
(defun convert-list-for-effect (list-of-csts inserter env system)
  (loop with rv
        for cst in list-of-csts
        do (setf rv (convert (cst:first cst) inserter env system))
        when (eq rv :no-return)
          return nil
        finally (return t)))

(defun convert-progn (sequence-cst inserter environment system)
  (if (cst:null sequence-cst)
      (convert-constant
       (cst:cst-from-expression nil :source (cst:source sequence-cst))
       inserter environment system)
      (loop for cst = sequence-cst then rest
            for this = (cst:first cst)
            for rest = (cst:rest cst)
            for rv = (convert this inserter environment system)
            when (cst:null rest) ; last form
              return rv
            ;; Some other form; return values ignored unless it never
            ;; returns, in which case we can just stop.
            when (eq :no-return rv)
              return rv)))
