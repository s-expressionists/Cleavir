(in-package #:cleavir-bir)

(defun map-iblock-instructions (f start-instruction)
  (check-type start-instruction instruction)
  (loop for i = start-instruction then (successor i)
        until (null i)
        do (funcall f i))
  (values))

(defun map-reachable-iblocks (f start)
  (check-type start iblock)
  ;; simple depth-first graph traversal
  ;; NOTE: Does not follow unwinds or recurse into functions
  (let ((seen (empty-set))
        (worklist (list start)))
    (loop for work = (pop worklist)
          until (null work)
          unless (presentp work seen)
            do (funcall f work)
               (setf seen (nset-adjoin work seen))
               (setf worklist (append (next (end work)) worklist))))
  (values))

(defun map-iblocks (f function)
  ;; This function may hit dead blocks if the set hasn't been GC'd.
  (check-type function function)
  (mapset f (iblocks function)))

(defun map-instructions (f function)
  (check-type function function)
  (let ((seen (empty-set))
        (worklist (list function)))
    (loop for work = (pop worklist)
          until (null work)
          unless (presentp work seen)
            do (setf seen (nset-adjoin work seen))
               (map-iblocks
                (lambda (ib)
                  (map-iblock-instructions
                   (lambda (i)
                     (typecase i
                       (enclose
                        (push (code i) worklist)))
                     (funcall f i))
                   ib))
                work)))
  (values))

(defun map-instructions-with-owner (f function)
  (check-type function function)
  (let ((seen (empty-set))
        (worklist (list function)))
    (loop for work = (pop worklist)
          for owner = work
          until (null work)
          unless (presentp work seen)
            do (setf seen (nset-adjoin work seen))
               (map-iblocks
                (lambda (ib)
                  (map-iblock-instructions
                   (lambda (i)
                     (typecase i
                       (enclose
                        (push (code i) worklist)))
                     (funcall f i owner))
                   ib))
                work)))
  (values))
