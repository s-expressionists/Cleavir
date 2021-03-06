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
        (linear-datum (cleavir-bir:output local-call)))
    (loop (typecase successor
            (cleavir-bir:jump
             ;; Make sure no dynamic environment actions happen.
             ;; FIXME: Maybe also ignore CATCH dynenvs.
             (if (cleavir-bir:unwindp successor)
                 (return successor)
                 (progn
                   (let ((inputs (cleavir-bir:inputs successor)))
                     (case (length inputs)
                       (0) ; nothing is fine; no value or control signficance
                       (1
                        (cond ((cleavir-bir:unused-p linear-datum)
                               (return successor)) ; value significance
                              ((eq linear-datum (first inputs))
                               ;; can keep looking
                               (setq linear-datum
                                     (first (cleavir-bir:outputs successor))))
                              (t ; value significance
                               (return successor))))
                       (t (return successor))) ; value significance
                     (setq successor (cleavir-bir:start
                                      (first (cleavir-bir:next successor))))))))
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
      (let* ((cont (logical-continuation call))
             (dynenv (cleavir-bir:dynamic-environment call))
             (call-out (cleavir-bir:output call))
             (use (cleavir-bir:transitive-use call-out))
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
                               :outputs (copy-list (cleavir-bir:inputs start))))
          (lambda-list (cleavir-bir:lambda-list (cleavir-bir:callee call))))
      (cleavir-bir:replace-terminator
       jump
       (cleavir-bir:end before))
      ;; The stuff in the AFTER block is now unreachable.
      (cleavir-bir:delete-iblock after)
      (let ((call-arguments (rest (cleavir-bir:inputs call)))
            (ftmd-arguments
              (loop for datum in (rest (cleavir-bir:inputs call))
                    collect (make-instance 'cleavir-bir:output
                              :name (cleavir-bir:name datum)
                              :derived-type (cleavir-bir:ctype datum))))
            (inputs '()))
        ;; Remove the local call.
        (cleavir-bir:delete-instruction call)
        ;; Insert fixed-to-multiple instructions to ensure only primary values
        ;; of the call arguments are used.
        (loop for arg in call-arguments for ftm-out in ftmd-arguments
              for ftm = (make-instance 'cleavir-bir:fixed-to-multiple
                          :inputs (list arg) :outputs (list ftm-out))
              do (cleavir-bir:insert-instruction-before ftm jump))
        ;; Compute inputs to the jump.
        (cleavir-bir:map-lambda-list
         (lambda (state item index)
           (declare (ignore index))
           (let ((arg (pop ftmd-arguments)))
             (ecase state
               (:required
                (push arg inputs))
               (&optional
                (let ((module (cleavir-bir:module (cleavir-bir:function call))))
                  (cond
                    (arg
                     (let* ((const (cleavir-bir:constant-in-module t module))
                            (suppliedp-out (make-instance 'cleavir-bir:output))
                            (suppliedp (make-instance
                                           'cleavir-bir:constant-reference
                                         :inputs (list const)
                                         :outputs (list suppliedp-out))))
                       (cleavir-bir:insert-instruction-before suppliedp jump)
                       (push arg inputs)
                       (push suppliedp-out inputs)))
                    (t
                     (let* ((nil-constant
                              (cleavir-bir:constant-in-module nil module))
                            (value-out (make-instance 'cleavir-bir:output))
                            (value (make-instance
                                       'cleavir-bir:constant-reference
                                     :inputs (list nil-constant)
                                     :outputs (list value-out)))
                            (suppliedp-out (make-instance 'cleavir-bir:output))
                            (suppliedp (make-instance
                                           'cleavir-bir:constant-reference
                                         :inputs (list nil-constant)
                                         :outputs (list suppliedp-out))))
                       (cleavir-bir:insert-instruction-before value jump)
                       (cleavir-bir:insert-instruction-before suppliedp jump)
                       (push value-out inputs)
                       (push suppliedp-out inputs))))))
               (&rest
                ;; The argument is unused, so we don't need to pass anything.
                ;; To keep the BIR consistent, we need to outright delete any
                ;; LETI, because otherwise it would refer to a now-undefined
                ;; ARGUMENT.
                (assert (cleavir-bir:unused-p item))
                (let ((use (cleavir-bir:use item)))
                  (when (typep use 'cleavir-bir:leti)
                    (cleavir-bir:delete-instruction use)))))))
         lambda-list)
        (setf (cleavir-bir:inputs jump) (nreverse inputs))))))

(defun rewire-return (function return-point-block)
  (let* ((returni (cleavir-bir:returni function))
         (outputs (cleavir-bir:inputs return-point-block))
         (jump (make-instance 'cleavir-bir:jump
                              :outputs (copy-list outputs)
                              :next (list return-point-block))))
    ;; THis assertion is sort of guaranteed by LOGICAL-CONTINUATION.
    (assert (<= (length (cleavir-bir:inputs return-point-block))
                (length (cleavir-bir:inputs returni))))
    (cleavir-bir:replace-terminator jump returni)
    (setf (cleavir-bir:inputs jump)
          (if (cleavir-bir:inputs return-point-block)
              (cleavir-bir:inputs returni)
              '()))))

(defun move-function-arguments-to-iblock (function)
  (let ((start (cleavir-bir:start function))
        (phis '()))
    (cleavir-bir:map-lambda-list
     (lambda (state item index)
       (declare (ignore index))
       (ecase state
         (:required
          (let ((supplied (make-instance 'cleavir-bir:phi :iblock start)))
            (push supplied phis)
            (cleavir-bir:replace-uses supplied item)))
         (&optional
          (let ((supplied (make-instance 'cleavir-bir:phi :iblock start))
                (supplied-p (make-instance 'cleavir-bir:phi :iblock start)))
            (push supplied phis)
            (push supplied-p phis)
            (cleavir-bir:replace-uses supplied (first item))
            (cleavir-bir:replace-uses supplied-p (second item))))
         (&rest
          ;; The argument is is unused, so don't do anything.
          (assert (cleavir-bir:unused-p item)))))
     (cleavir-bir:lambda-list function))
    (setf (cleavir-bir:inputs start) (nreverse phis))))

;;; Integrate the blocks of FUNCTION into TARGET-FUNCTION in the
;;; dynamic environment DYNENV.
(defun interpolate-function (function target-function dynenv)
  (cleavir-set:nunionf (cleavir-bir:variables target-function)
                       (cleavir-bir:variables function))
  (cleavir-set:nunionf (cleavir-bir:catches target-function)
                       (cleavir-bir:catches function))
  ;; Re-home iblocks (and indirectly, instructions), and if the
  ;; function unwinds to its target function, change it to a local
  ;; unwind.
  (cleavir-bir:do-iblocks (ib function)
    (let ((u (cleavir-bir:end ib)))
      (when (typep u 'cleavir-bir:unwind)
        (when (eq (cleavir-bir:function (cleavir-bir:destination u))
                  target-function)
          (replace-unwind u))))
    (when (eq (cleavir-bir:dynamic-environment ib) function)
      (setf (cleavir-bir:dynamic-environment ib)
            dynenv))
    (setf (cleavir-bir:function ib) target-function))
  ;; FUNCTION no longer owns these blocks. Need to do this so triggers
  ;; don't accidentally clean them up.
  (setf (cleavir-bir:start function) nil))

;;; If there is a common return point, integrate FUNCTION into the
;;; graph of TARGET-OWNER and rewire the calls into the body of the
;;; FUNCTION.
;;; If the return continuation is unknown, it can still get contified
;;; as long as the local function never returns normally.
;;; When the function does return normally, wire the return value of
;;; the function into the common ``transitive'' use of the local calls.
(defun contify (function local-calls return-point common-use common-dynenv target-owner)
  (let* ((returni (cleavir-bir:returni function))
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
                 (let ((dummy-block (nth-value 1 (cleavir-bir:split-block-after unique-call)))
                       (ucall-out (cleavir-bir:output unique-call)))
                   (unless (cleavir-bir:unused-p ucall-out)
                     (let ((phi (make-instance 'cleavir-bir:phi :iblock dummy-block)))
                       (setf (cleavir-bir:inputs dummy-block) (list phi))
                       ;; Replace the call-as-datum with the return-values.
                       (cleavir-bir:replace-uses phi ucall-out)))
                   dummy-block))
               (if (eq return-point :unknown)
                   :unknown
                   (cleavir-bir:iblock return-point))))
         (start (cleavir-bir:start function)))
    (unless (and returni (eq return-point :unknown))
      (move-function-arguments-to-iblock function)
      (unless (eq return-point :unknown)
        (when returni
          (rewire-return function return-point)))
      (interpolate-function function
                            target-owner
                            common-dynenv)
      (cleavir-set:doset (call local-calls)
        (rewire-call-into-body call start))
      ;; Recompute the flow order, as now the iblocks of the function
      ;; have been integrated into that of TARGET-OWNER.
      (cleavir-bir:compute-iblock-flow-order target-owner)
      ;; Merge the blocks. Merge the tail first since the
      ;; interpolated function might just be one block.
      (when returni
        (cleavir-bir:merge-successor-if-possible (cleavir-bir:iblock returni)))
      (when unique-call
        (cleavir-bir:merge-successor-if-possible (cleavir-bir:iblock unique-call)))
      ;; We've interpolated and there are potentially useless
      ;; catches in TARGET-OWNER, so now that the IR is in a
      ;; consistent state, eliminate them.
      (eliminate-catches target-owner)
      t)))

;;; We can inline required, optional, and ignored &rest parameters.
(defun lambda-list-too-hairy-p (lambda-list)
  (let ((too-hairy-p nil))
    (cleavir-bir:map-lambda-list
     (lambda (state item index)
       (declare (ignore index))
       (case state
         ((:required &optional))
         (&rest
          (setq too-hairy-p (not (cleavir-bir:unused-p item))))
         (t
          (setq too-hairy-p t))))
     lambda-list)
    too-hairy-p))

(defun maybe-interpolate (function)
  ;; When a function has no enclose and returns to a single control
  ;; point, it is eligible for interpolation.
  (when (and (null (cleavir-bir:enclose function))
             (not (lambda-list-too-hairy-p
                   (cleavir-bir:lambda-list function))))
    ;; FIXME: We should respect inline and not inline declarations.
    (let ((local-calls (cleavir-bir:local-calls function)))
      (unless (or (cleavir-set:empty-set-p local-calls)
                  (cleavir-set:some
                   (lambda (c) (typep c 'cleavir-bir:mv-local-call))
                   local-calls))
        (multiple-value-bind (return-point common-use common-dynenv target-owner)
            (common-return-cont function local-calls)
          ;; Per BIR rules we can't really interpolate any function
          ;; when it's ambiguous what its dynenv or owners should be.
          (when (and common-dynenv target-owner)
            ;; FIXME: We should really be deleting functions which are
            ;; only local called by themselves, but the BIR invariants
            ;; are hard to maintain from outside the BIR system
            ;; itself.
            (unless (eq function target-owner)
              (contify function local-calls return-point common-use common-dynenv target-owner))))))))
