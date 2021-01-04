(in-package #:cleavir-bir)

(defmacro do-functions ((function module) &body body)
  `(cleavir-set:doset (,function (functions ,module))
     ,@body))

(defun map-functions (f module)
  (do-functions (function module)
    (funcall f function)))

(defmacro do-iblocks ((iblock function &optional (direction :forward)) &body body)
  (multiple-value-bind (from to)
      (ecase direction
        (:forward (values 'start 'next))
        (:backward (values 'tail 'prev)))
    `(do ((,iblock (,from ,function) (,to ,iblock)))
         ((null ,iblock))
       ,@body)))

(defun map-iblocks (f function)
  (do-iblocks (iblock function)
    (funcall f iblock)))

(defmacro do-iblock-instructions ((instruction from &optional (direction :forward))
                                  &body body)
  `(loop for ,instruction = ,from then (,(ecase direction
                                           (:forward 'successor)
                                           (:backward 'predecessor))
                                        ,instruction)
         until (null ,instruction)
         do (progn ,@body)))

(defun map-iblock-instructions (f start-instruction)
  (check-type start-instruction instruction)
  (do-iblock-instructions (instruction start-instruction)
    (funcall f instruction))
  (values))

(defun map-iblock-instructions-backwards (f end-instruction)
  (check-type end-instruction instruction)
  (do-iblock-instructions (instruction end-instruction :backward)
    (funcall f instruction))
  (values))

;;; Map all instructions owned by the given function
(defun map-local-instructions (f function)
  (do-iblocks (iblock function)
    (map-iblock-instructions f (start iblock))))

;;; This utility parses BIR lambda lists. FUNCTION takes three
;;; arguments: The state of the parse (e.g. &OPTIONAL), the current
;;; lambda-list item being parsed, and the index of the item.
(defun map-lambda-list (function lambda-list)
  (let ((state :required)
        (index 0))
    (dolist (item lambda-list)
      (if (symbolp item)
          (setq state item)
          (progn
            (funcall function state item index)
            (incf index))))))
