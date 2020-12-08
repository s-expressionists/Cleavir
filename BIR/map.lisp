(in-package #:cleavir-bir)

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

(defun map-reachable-iblocks (f start)
  (check-type start iblock)
  ;; pre-order depth-first graph traversal
  ;; NOTE: Does not follow unwinds or recurse into functions
  (let ((seen (cleavir-set:empty-set))
        (worklist (list start)))
    (loop for work = (pop worklist)
          until (null work)
          unless (cleavir-set:presentp work seen)
            do (funcall f work)
               (setf seen (cleavir-set:nadjoin work seen))
               (setf worklist (append (next (end work)) worklist))))
  (values))

(defun map-iblocks (f function)
  ;; This function may hit dead blocks if the set hasn't been refreshed.
  (check-type function function)
  (cleavir-set:mapset nil f (iblocks function)))

(defun map-iblocks-postorder (f function)
  (let ((seen (cleavir-set:make-set)))
    (labels ((traverse (iblock)
               (unless (cleavir-set:presentp iblock seen)
                 (cleavir-set:nadjoinf seen iblock)
                 (mapc #'traverse (successors iblock))
                 (funcall f iblock))))
      (traverse (start function)))
    (values)))

;;; Forward flow order is the preferred order for forward flow
;;; dataflow analyses, since predecessors are ordered before
;;; successors.
(defun iblocks-forward-flow-order (function)
  (let ((result '()))
    (map-iblocks-postorder (lambda (iblocks)
                             (push iblocks result))
                           function)
    result))

;;; Map all instructions owned by the given function
(defun map-local-instructions (f function)
  (check-type function function)
  (map-iblocks (lambda (ib) (map-iblock-instructions f (start ib))) function))

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
