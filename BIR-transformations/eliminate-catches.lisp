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
       :inputs () :outputs () :next (list normal-next))
     catch)
    ;; Fix reachability
    (cleavir-bir:compute-iblock-flow-order (cleavir-bir:function fore))
    ;; Merge if able
    ;; NOTE: It may be possible to merge blocks within the tagbody as well,
    ;; but it's slightly complicated to do so correctly while not merging
    ;; deleted iblocks, and anyway practically speaking meta-evaluate will
    ;; handle it.
    (loop while (cleavir-bir:merge-successor-if-possible fore))))

(defun eliminate-catches (function)
  (cleavir-set:doset (catch (cleavir-bir:catches function))
    (when (catch-eliminable-p catch)
      (eliminate-catch catch))))

(defun module-eliminate-catches (module)
  (cleavir-bir:map-functions #'eliminate-catches module))
