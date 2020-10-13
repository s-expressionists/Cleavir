(in-package #:cleavir-bir-transformations)

(defun catch-eliminable-p (catch)
  (cleavir-set:empty-set-p (cleavir-bir:unwinds catch)))

(defun eliminate-catch (catch)
  (let ((nde (cleavir-bir:dynamic-environment catch))
        (fore (cleavir-bir:iblock catch))
        (normal-next (first (cleavir-bir:next catch)))
        (other-next (rest (cleavir-bir:next catch))))
    (cleavir-set:doset (s (cleavir-bir:scope catch))
      (setf (cleavir-bir:dynamic-environment s) nde))
    ;; Replace the instruction
    (cleavir-bir:replace-terminator
     (make-instance 'cleavir-bir:jump
       :inputs () :outputs () :unwindp nil
       :next (list normal-next))
     catch)
    ;; Other blocks might be unreachable now
    (mapc #'cleavir-bir:maybe-delete-iblock other-next)
    ;; Merge if able
    (when (cleavir-bir:iblocks-mergable-p fore normal-next)
      (cleavir-bir:merge-iblocks fore normal-next))))

(defun eliminate-catches (function)
  (cleavir-bir:map-iblocks
   (lambda (ib)
     (let ((end (cleavir-bir:end ib)))
       (when (and (typep end 'cleavir-bir:catch)
                  (catch-eliminable-p end))
         (eliminate-catch end))))
   function))

(defun module-eliminate-catches (module)
  (cleavir-set:mapset nil #'eliminate-catches
                      (cleavir-bir:functions module)))
