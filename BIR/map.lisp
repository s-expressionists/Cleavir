(in-package #:cleavir-bir)

(defmacro do-functions ((function module) &body body)
  "Execute the BODY with FUNCTION bound to each function in MODULE. Arbitrary order."
  `(set:doset (,function (functions ,module)) ,@body))

(defun map-functions (f module)
  "Call F on each function in MODULE. Arbitrary order."
  (do-functions (function module)
    (funcall f function)))

(defmacro do-iblocks ((iblock function &optional (direction :forward)) &body body)
  "Execute the BODY with IBLOCK bound to each iblock in FUNCTION.
DIRECTION may be :FORWARD for forward flow order, or :BACKWARD for reverse flow order."
  (multiple-value-bind (from to)
      (ecase direction
        (:forward (values 'start '%next))
        (:backward (values 'tail '%prev)))
    `(do ((,iblock (,from ,function) (,to ,iblock)))
         ((null ,iblock))
       ,@body)))

(defun map-iblocks (f function)
  "Call F on each iblock in FUNCTION, in forward flow order."
  (do-iblocks (iblock function)
    (funcall f iblock)))

(defmacro do-iblock-instructions ((instruction iblock
                                   &optional (direction :forward))
                                  &body body)
  "Execute the BODY with INSTRUCTION bound to each instruction in IBLOCK.
DIRECTION may be :FORWARD or :BACKWARD."
  (multiple-value-bind (from to)
      (ecase direction
        (:forward (values 'start 'successor))
        (:backward (values 'end 'predecessor)))
    `(do ((,instruction (,from ,iblock) (,to ,instruction)))
         ((null ,instruction) (values))
       ,@body)))

(defun map-iblock-instructions (f iblock)
  "Call F on each instruction in IBLOCK, in forward order."
  (check-type iblock iblock)
  (do-iblock-instructions (instruction iblock)
    (funcall f instruction)))

(defun map-iblock-instructions-backwards (f iblock)
  "Call F on each instruction in IBLOCK, in backwards order."
  (check-type iblock iblock)
  (do-iblock-instructions (instruction iblock :backward)
    (funcall f instruction)))

(defun map-local-instructions (f function)
  "Call F on all instructions owned by FUNCTION, in forward flow order."
  (do-iblocks (iblock function)
    (map-iblock-instructions f iblock)))

(defun map-lambda-list (function lambda-list)
  "This utility parses BIR lambda lists. FUNCTION takes three arguments: The state of the parse (e.g. &OPTIONAL), the current lambda-list item being parsed, and the index of the item."
  (let ((state :required)
        (index 0))
    (dolist (item lambda-list)
      (if (symbolp item)
          (setq state item)
          (progn
            (funcall function state item index)
            (incf index))))))
