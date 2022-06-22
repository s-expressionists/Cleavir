(in-package #:cleavir-abstract-interpreter)

;;; Forward control infos are conceptually associated with a control edge.
;;; We represent this by having each instruction's info be a list of infos, one for
;;; each of its successors; we also define that if there's only one successor there's
;;; no list, to save some consing.
;;; There is only ever one input to FLOW-INSTRUCTION; this is the JOIN of the infos
;;; from all predecessor edges.

;;; We don't need to do anything to initialize entries, as instruction-input-info
;;; will compute input control data from entry points itself.
(defmethod initialize-entry-point ((strategy strategy) (domain forward-control)
                                   (function bir:function)))

(defmethod initialize-instruction ((strategy optimism) (domain forward-control)
                                   (inst bir:instruction))
  (setf (info strategy domain inst)
        (if (or (bir:successor inst) (= (length (bir:next inst)) 1))
            (infimum domain)
            (make-list (length (bir:next inst)) :initial-element (infimum domain)))))

(defmethod initialize-instruction ((strategy pessimism) (domain forward-control)
                                   (inst bir:instruction))
  (setf (info strategy domain inst)
        (if (or (bir:successor inst) (= (length (bir:next inst)) 1))
            (supremum domain)
            (make-list (length (bir:next inst)) :initial-element (supremum domain)))))

(defmethod instruction-input-info ((strategy strategy) (domain forward-control)
                                   (instruction bir:instruction))
  (let ((pred (bir:predecessor instruction))
        (ib (bir:iblock instruction))
        (function (bir:function instruction)))
    (cond (pred (info strategy domain pred)) ; we're in an iblock
          ((not (eq ib (bir:start function)))
           ;; We're the start of an iblock, and that iblock is not the start of its
           ;; function, so just use the predecessors.
           (let ((accum (infimum domain)))
             (set:doset (pred (bir:predecessors ib))
               (let* ((end (bir:end pred))
                      (end-next (bir:next end))
                      (infos (info strategy domain end))
                      (info (if (= (length end-next) 1)
                                infos
                                (nth (position ib end-next) infos))))
                 (setf accum (join domain accum info))))
             (set:doset (pred (bir:entrances ib))
               (let* ((end (bir:end pred))
                      ;; UNWINDs only have the one successor.
                      (info (info strategy domain end)))
                 (setf accum (join domain accum info))))
             accum))
          ((or (bir:enclose function) (set:empty-set-p (bir:local-calls function)))
           ;; We're at the start of a function and it can be called from anywhere.
           (supremum domain))
          (t ; We're at the start of a function and we understand where it's called.
           (let ((accum (infimum domain)))
             (set:doset (call (bir:local-calls function) accum)
               ;; Recurse; this lets us account for calls at the head of blocks, etc.
               (let ((info (instruction-input-info strategy domain call)))
                 (setf accum (join domain accum info)))))))))

(defgeneric maybe-mark-control (strategy domain instruction successor old-info new-info))

(defmethod instruction-output-info ((strategy strategy) (domain forward-control)
                                    (instruction bir:instruction) &rest new-infos)
  (let ((old-info (info strategy domain instruction))
        (succ (bir:successor instruction)))
    (if succ
        ;; We're in a block. Easy case.
        (setf (info strategy domain instruction)
              (maybe-mark-control strategy domain instruction succ
                                  old-info (first new-infos)))
        (let ((next (bir:next instruction)))
          (if (= (length next) 1)
              (setf (info strategy domain instruction)
                    (maybe-mark-control strategy domain instruction (first next)
                                        old-info (first new-infos)))
              ;; Multiple successors: instruction's info is a list.
              (map-into old-info
                        (lambda (old-info new-info successor)
                          (maybe-mark-control strategy domain instruction successor
                                              old-info new-info))
                        old-info new-infos next))))))

(defmethod instruction-output-info ((strategy strategy) (domain forward-control)
                                    (instruction bir:unwind) &rest new-infos)
  (let ((old-info (info strategy domain instruction))
        (new-info (first new-infos)))
    (setf (info strategy domain instruction)
          (maybe-mark-control strategy domain instruction (bir:destination instruction)
                              old-info new-info))))

(defmethod instruction-output-info ((strategy strategy) (domain forward-control)
                                    (instruction bir:returni) &rest new-infos)
  (let ((new-info (first new-infos)))
    (set:doset (call (bir:local-calls (bir:function instruction)))
      (let ((old-info (info strategy domain call)))
        (setf (info strategy domain call)
              (maybe-mark-control strategy domain call (bir:successor call)
                                  old-info new-info))))))

;;; dumb default that should be enough to ensure termination:
;;; widen if we start a block (as there could be a loop)
;;; slightly less dumb would be checking for multiple predecessors.
(defmethod widening-point-p ((strategy strategy) (thing bir:instruction))
  (null (bir:predecessor thing)))

(defmethod maybe-mark-control ((strategy optimism) domain instruction successor old new)
  (let ((new (join domain old new)))
    (multiple-value-bind (sub surety) (subinfop domain new old)
      (cond ((and surety (not sub))
             (mark strategy successor)
             (if (widening-point-p strategy instruction)
                 (widen domain old new)
                 new))
            (t new)))))

(defmethod maybe-mark-control ((strategy pessimism) domain instruction successor old new)
  (let ((new (meet domain old new)))
    (multiple-value-bind (sub surety) (subinfop domain old new)
      (when (and surety (not sub))
        (mark strategy successor)))
    new))
