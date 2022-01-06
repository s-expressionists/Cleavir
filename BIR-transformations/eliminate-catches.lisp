(in-package #:cleavir-bir-transformations)

(defun catch-eliminable-p (catch) (set:empty-set-p (bir:unwinds catch)))

(defun eliminate-catch (catch)
  (let ((fore (bir:iblock catch))
        (normal-next (first (bir:next catch))))
    ;; Replace the instruction
    (bir:replace-terminator
     (make-instance 'bir:jump
       :inputs () :outputs () :next (list normal-next)
       :origin (bir:origin catch) :policy (bir:policy catch))
     catch)
    ;; Fix reachability
    (bir:compute-iblock-flow-order (bir:function fore))
    ;; Merge if able
    ;; NOTE: It may be possible to merge blocks within the tagbody as well,
    ;; but it's slightly complicated to do so correctly while not merging
    ;; deleted iblocks, and anyway practically speaking meta-evaluate will
    ;; handle it.
    (loop while (bir:merge-successor-if-possible fore))))

(defun eliminate-catches (function)
  (set:doset (catch (bir:catches function))
    (when (catch-eliminable-p catch)
      (eliminate-catch catch))))

(defun module-eliminate-catches (module)
  (bir:map-functions #'eliminate-catches module))
