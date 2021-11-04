(in-package #:cleavir-bir)

(defmacro do-functions ((function module) &body body)
  `(set:doset (,function (functions ,module)) ,@body))

(defun map-functions (f module)
  (do-functions (function module)
    (funcall f function)))

(defmacro do-iblocks ((iblock function &optional (direction :forward)) &body body)
  (multiple-value-bind (from to)
      (ecase direction
        (:forward (values 'start '%next))
        (:backward (values 'tail '%prev)))
    `(do ((,iblock (,from ,function) (,to ,iblock)))
         ((null ,iblock))
       ,@body)))

(defun map-iblocks (f function)
  (do-iblocks (iblock function)
    (funcall f iblock)))

(defmacro do-iblock-instructions ((instruction iblock
                                   &optional (direction :forward))
                                  &body body)
  (multiple-value-bind (from to)
      (ecase direction
        (:forward (values 'start 'successor))
        (:backward (values 'end 'predecessor)))
    `(do ((,instruction (,from ,iblock) (,to ,instruction)))
         ((null ,instruction) (values))
       ,@body)))

(defun map-iblock-instructions (f iblock)
  (check-type iblock iblock)
  (do-iblock-instructions (instruction iblock)
    (funcall f instruction)))

(defun map-iblock-instructions-backwards (f iblock)
  (check-type iblock iblock)
  (do-iblock-instructions (instruction iblock :backward)
    (funcall f instruction)))

;;; Map all instructions owned by the given function
(defun map-local-instructions (f function)
  (do-iblocks (iblock function)
    (map-iblock-instructions f iblock)))
