(in-package #:cleavir-bir)

(defun map-iblock-instructions (f start-instruction)
  (check-type start-instruction instruction)
  (loop for i = start-instruction then (successor i)
        until (null i)
        do (funcall f i))
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

;;; Arbitrary order.
(defun map-instructions (f function)
  (check-type function function)
  (let ((seen (cleavir-set:empty-set))
        (worklist (list function)))
    (loop for work = (pop worklist)
          until (null work)
          unless (cleavir-set:presentp work seen)
            do (cleavir-set:nadjoinf seen work)
               (map-local-instructions
                (lambda (i)
                  (typecase i (enclose (push (code i) worklist)))
                  (funcall f i))
                work)))
  (values))

;;; Arbitrary order
(defun map-instructions-with-owner (f function)
  (check-type function function)
  (let ((seen (cleavir-set:empty-set))
        (worklist (list function)))
    (loop for work = (pop worklist)
          for owner = work
          until (null work)
          unless (cleavir-set:presentp work seen)
            do (cleavir-set:nadjoinf seen work)
               (map-local-instructions
                (lambda (i)
                  (typecase i (enclose (push (code i) worklist)))
                  (funcall f i owner))
                work)))
  (values))

;; Given a set of functions, do m-i-w-o
(defun map-instructions-with-owner-from-set (f function-set)
  (check-type function-set cleavir-set:set)
  (cleavir-set:doset (function function-set)
    (map-local-instructions
     (lambda (i) (funcall f i function))
     function)))
