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
    ;; Other blocks might be unreachable now
    (mapc #'cleavir-bir:maybe-delete-iblock other-next)
    ;; Merge if able
    (cleavir-bir:merge-successor-if-possible fore)
    ;; Fix reachability
    (cleavir-bir:refresh-local-iblocks (cleavir-bir:function fore))))

(defun eliminate-catches (function)
  (cleavir-set:doset (catch (cleavir-bir:catches function))
    (when (catch-eliminable-p catch)
      (eliminate-catch catch))))

(defun module-eliminate-catches (module)
  (cleavir-bir:map-functions #'eliminate-catches module))
