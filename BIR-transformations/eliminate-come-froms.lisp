(in-package #:cleavir-bir-transformations)

(defun come-from-eliminable-p (come-from)
  (set:empty-set-p (bir:unwinds come-from)))

(defun eliminate-come-from (come-from)
  (let ((fore (bir:iblock come-from))
        (normal-next (first (bir:next come-from))))
    ;; Replace the instruction
    (bir:replace-terminator
     (make-instance 'bir:jump
       :inputs () :outputs () :next (list normal-next)
       :origin (bir:origin come-from) :policy (bir:policy come-from))
     come-from)
    ;; Fix reachability
    (bir:compute-iblock-flow-order (bir:function fore))
    ;; Merge if able
    ;; NOTE: It may be possible to merge blocks within the tagbody as well,
    ;; but it's slightly complicated to do so correctly while not merging
    ;; deleted iblocks, and anyway practically speaking meta-evaluate will
    ;; handle it.
    (loop while (bir:merge-successor-if-possible fore))))

(defun eliminate-come-froms (function)
  (set:doset (come-from (bir:come-froms function))
    (when (come-from-eliminable-p come-from)
      (eliminate-come-from come-from))))

(defun module-eliminate-come-froms (module)
  (bir:map-functions #'eliminate-come-froms module))
