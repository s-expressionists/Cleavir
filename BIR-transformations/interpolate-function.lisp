(in-package #:cleavir-bir-transformations)

;;; This is a basically the A_call analysis in Fluet & Weeks.  When we
;;; are tail recursing, simply return the callee as a sentinel.  It
;;; would be nice to use the A_dom analysis, as that is provably
;;; optimal, and generalizes to all tail calls, not just tail
;;; recursive ones.
(defun logical-continuation (local-call)
  ;; We need to skip over instructions and jumps with no control flow
  ;; significance. If the jump is carrying a value via phi that is not
  ;; the value of the local-call, we also quit.
  (let ((successor (cleavir-bir:successor local-call))
        (linear-datum local-call))
    (loop (typecase successor
            (cleavir-bir:jump
             ;; Make sure no dynamic environment actions happen.
             ;; FIXME: Maybe also ignore CATCH dynenvs.
             (if (cleavir-bir:unwindp successor)
                 (return successor)
                 (progn
                   (let ((inputs (cleavir-bir:inputs successor)))
                     (case (length inputs)
                       (0 ; nothing is fine; no value or control signficance
                        )
                       (1
                        (if (cleavir-bir:unused-p linear-datum)
                            (return successor) ; value significance
                            (if (eq linear-datum (first inputs))
                                (setq linear-datum (first (cleavir-bir:outputs successor))) ; can keep looking
                                (return successor) ; value significance
                                )))
                       (t (return successor)) ; value significance
                       )
                     (setq successor (cleavir-bir:start (first (cleavir-bir:next successor))))))))
            (cleavir-bir:returni
             ;; Is this a self tail recursive call?
             (let ((callee (cleavir-bir:callee local-call)))
               (return (if (eq (cleavir-bir:function successor)
                               callee)
                           callee
                           successor))))
            (t (return successor))))))

;;; Think about where each local call must ultimately return to. Make
;;; sure the dynamic environments of all the calls are the same so the
;;; cleanup actions are also the same. Also make sure the transitive
;;; use of each call is actually the same.
(defun common-return-cont (function calls)
  (let ((return-point :uncalled)
        common-use
        common-dynenv
        target-owner)
    (cleavir-set:doset (call calls)
      (let ((cont (logical-continuation call))
            (dynenv (cleavir-bir:dynamic-environment call))
            (use (cleavir-bir:transitive-use call))
            (owner (cleavir-bir:function call)))
        ;; Check which properties every user shares.
        (cond ((eq return-point :uncalled)
               (unless (eq function cont)
                 (setf common-dynenv dynenv)
                 (setf return-point cont)
                 (setf common-use use)
                 (setf target-owner owner)))
              ((eq cont function))
              (t
               (unless (eq return-point cont)
                 (setq return-point :unknown))
               (unless (eq dynenv common-dynenv)
                 (setq dynenv nil))
               (unless (eq use common-use)
                 (setq use nil))
               (unless (eq owner target-owner)
                 (setq target-owner nil))))))
    (values return-point common-use common-dynenv target-owner)))

(defun replace-unwind (u)
  (let ((unwind-inputs (cleavir-bir:inputs u))
        (unwind-outputs (cleavir-bir:outputs u))
        (new (make-instance 'cleavir-bir:jump
               :next (list (cleavir-bir:destination u)))))
    (cleavir-bir:replace-terminator new u)
    (setf (cleavir-bir:inputs new) unwind-inputs
          (cleavir-bir:outputs new) unwind-outputs)))

;;; Rewire control from the local call into the function body.
(defun rewire-call-into-body (call start)
  ;; Now jump with arguments into START.
  (multiple-value-bind (before after)
      (cleavir-bir:split-block-after call)
    (let ((jump (make-instance 'cleavir-bir:jump
                               :next (list start)
                               :outputs (cleavir-bir:inputs start))))
      (cleavir-bir:replace-terminator
       jump
       (cleavir-bir:end before))
      ;; The stuff in the AFTER block is now unreachable.
      (cleavir-bir:delete-iblock after)
      ;; Remove the local call.
      (cleavir-bir:delete-computation call)
      (setf (cleavir-bir:inputs jump)
            (rest (cleavir-bir:inputs call))))))

(defun rewire-return (function return-point-block)
  ;; Replace the return instruction with a jump if there is one, or else
  ;; delete AFTER and any later straight line blocks.
  (let* ((returni (cleavir-bir:end (cleavir-bir:end function)))
         (outputs (cleavir-bir:inputs return-point-block))
         (jump (make-instance 'cleavir-bir:jump
                              :outputs outputs
                              :next (list return-point-block))))
    ;; THis assertion is sort of guaranteed by LOGICAL-CONTINUATION.
    (assert (funcall '<=
                     (length (cleavir-bir:inputs return-point-block))
                     (length (cleavir-bir:inputs returni))))
    (cleavir-bir:replace-terminator jump returni)
    (setf (cleavir-bir:inputs jump)
          (if (cleavir-bir:inputs return-point-block)
              (cleavir-bir:inputs returni)
              '()))))

;;; Integrate the blocks of FUNCTION into TARGET-FUNCTION in the
;;; dynamic environment DYNENV. Move the arguments of FUNCTION into
;;; its start block.
(defun interpolate-function (function target-function dynenv)
  (cleavir-set:nunionf (cleavir-bir:variables target-function)
                       (cleavir-bir:variables function))
  (cleavir-set:nunionf (cleavir-bir:catches target-function)
                       (cleavir-bir:catches function))
  (let* ((start (cleavir-bir:start function))
         (phis (mapcar (lambda (argument)
                         (assert (typep argument 'cleavir-bir:argument))
                         (make-instance 'cleavir-bir:phi :iblock start))
                       (cleavir-bir:lambda-list function))))
    (setf (cleavir-bir:inputs start) phis)
    (mapc #'cleavir-bir:replace-uses phis (cleavir-bir:lambda-list function)))
  ;; If the function unwinds to their target function, change it to a
  ;; local unwind.
  (cleavir-set:doset (ib (cleavir-bir:exits function))
    (let ((u (cleavir-bir:end ib)))
      (check-type u cleavir-bir:unwind)
      (when (eq (cleavir-bir:function (cleavir-bir:destination u))
                target-function)
        (replace-unwind u))))
  ;; Re-home iblocks (and indirectly, instructions)
  (cleavir-bir:map-iblocks
   (lambda (ib)
     (when (eq (cleavir-bir:dynamic-environment ib) function)
       (setf (cleavir-bir:dynamic-environment ib)
             dynenv))
     (setf (cleavir-bir:function ib) target-function)
     (cleavir-set:nadjoinf (cleavir-bir:iblocks target-function) ib))
   function)
  ;; FUNCTION no longer owns these blocks. Need to do this so triggers
  ;; don't accidentally clean them up.
  (setf (cleavir-bir:iblocks function) (cleavir-set:empty-set)))

;;; If there is a common return point, integrate FUNCTION into the
;;; graph of TARGET-OWNER and rewire the calls into the body of the
;;; FUNCTION.
;;; If the return continuation is unknown, it can still get contified
;;; as long as the local function never returns normally.
;;; When the function does return normally, wire the return value of
;;; the function into the common ``transitive'' use of the local calls.
(defun contify (function local-calls return-point common-use common-dynenv target-owner)
  (let* ((end (cleavir-bir:end function))
         ;; If the return-point has a predecessor, it does not start a
         ;; block and will be the unique outside call to this
         ;; function, which means we should normalize the return point
         ;; to be a dummy block.
         (unique-call
           (and (not (eq return-point :unknown))
                (cleavir-bir:predecessor return-point)))
         (return-point
           (if unique-call
               (progn
                 (check-type unique-call cleavir-bir:local-call)
                 (let ((dummy-block (nth-value 1 (cleavir-bir:split-block-after unique-call))))
                   (unless (cleavir-bir:unused-p unique-call)
                     (let ((phi (make-instance 'cleavir-bir:phi :iblock dummy-block)))
                       (setf (cleavir-bir:inputs dummy-block) (list phi))
                       ;; Replace the call-as-datum with the return-values.
                       (cleavir-bir:replace-uses phi unique-call)))
                   dummy-block))
               (if (eq return-point :unknown)
                   :unknown
                   (cleavir-bir:iblock return-point)))))
    (unless (and end (eq return-point :unknown))
      (interpolate-function function
                            target-owner
                            common-dynenv)
      (unless (eq return-point :unknown)
        (if end
            (rewire-return function return-point)
            ;; FIXME: Subtly wrong because of loops and may be unreachable.
            (cleavir-bir:maybe-delete-iblock return-point)))
      (cleavir-set:doset (call local-calls)
        (rewire-call-into-body call (cleavir-bir:start function))))
    ;; Merge the blocks. Merge the tail first since the
    ;; interpolated function might just be one block.
    (if end
        (cleavir-bir:merge-successor-if-possible end)
        ;; The function doesn't return, so make sure later blocks are deleted
        (cleavir-bir:refresh-local-iblocks target-owner))
    (when unique-call
      (cleavir-bir:merge-successor-if-possible (cleavir-bir:iblock unique-call))
      ;; TODO: can generalize this to the case of more than
      ;; one outside call.
      ;; If we have a FTM->MTF sequence, clear it out.
      (when (and common-use
                 (typep common-use 'cleavir-bir:multiple-to-fixed))
        (let ((definition (first (cleavir-bir:inputs common-use))))
          (when (typep definition 'cleavir-bir:fixed-to-multiple)
            (cleavir-bir:delete-transmission definition common-use))))))
  ;; We've interpolated and there are potentially useless
  ;; catches in TARGET-OWNER, so now that the IR is in a
  ;; consistent state, eliminate them.
  (eliminate-catches target-owner))

(defun maybe-interpolate (function)
  ;; When a function has no encloses and returns to a single control
  ;; point, it is eligible for interpolation.
  (when (cleavir-set:empty-set-p (cleavir-bir:encloses function))
    ;; FIXME: We should respect inline and not inline declarations.
    (let ((local-calls (cleavir-bir:local-calls function)))
      (unless (cleavir-set:empty-set-p local-calls)
        (multiple-value-bind (return-point common-use common-dynenv target-owner)
            (common-return-cont function local-calls)
          (assert (not (eq return-point :uncalled)))
          ;; Per BIR rules we can't really interpolate any function
          ;; when it's ambiguous what its dynenv or owners should be.
          (when (and common-dynenv target-owner)
            (contify function local-calls return-point common-use common-dynenv target-owner)))))))
