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
  (let ((successor (bir:successor local-call))
        (linear-datum (bir:output local-call)))
    (loop (typecase successor
            (bir:jump
             ;; Make sure no dynamic environment actions happen.
             ;; FIXME: Maybe also ignore CATCH dynenvs.
             (if (bir:unwindp successor)
                 (return successor)
                 (progn
                   (let ((inputs (bir:inputs successor)))
                     (case (length inputs)
                       (0) ; nothing is fine; no value or control signficance
                       (1
                        (cond ((bir:unused-p linear-datum)
                               (return successor)) ; value significance
                              ((eq linear-datum (first inputs))
                               ;; can keep looking
                               (setq linear-datum
                                     (first (bir:outputs successor))))
                              (t ; value significance
                               (return successor))))
                       (t (return successor))) ; value significance
                     (setq successor (bir:start
                                      (first (bir:next successor))))))))
            (bir:returni
             ;; Is this a self tail recursive call?
             (let ((callee (bir:callee local-call)))
               (return (if (eq (bir:function successor) callee)
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
    (set:doset (call calls)
      (let* ((cont (logical-continuation call))
             (dynenv (bir:dynamic-environment call))
             (call-out (bir:output call))
             (use (bir:transitive-use call-out))
             (owner (bir:function call)))
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
                 (setq common-dynenv nil))
               (unless (eq use common-use)
                 (setq common-use nil))
               (unless (eq owner target-owner)
                 (setq target-owner nil))))))
    (values return-point common-use common-dynenv target-owner)))

(defun replace-unwind (u)
  (let ((unwind-inputs (bir:inputs u))
        (unwind-outputs (bir:outputs u))
        (new (make-instance 'bir:jump
               :next (list (bir:destination u))
               :origin (bir:origin u)
               :policy (bir:policy u))))
    (bir:replace-terminator new u)
    (setf (bir:inputs new) unwind-inputs
          (bir:outputs new) unwind-outputs)))

;;; Rewire control from the local call into the function body.
(defun rewire-call-into-body (call start)
  ;; Now jump with arguments into START.
  (multiple-value-bind (before after)
      (bir:split-block-after call)
    (let* ((origin (bir:origin call))
           (policy (bir:policy call))
           (jump (make-instance 'bir:jump
                   :next (list start)
                   :outputs (copy-list (bir:inputs start))
                   :origin origin :policy policy))
           (lambda-list (bir:lambda-list (bir:callee call))))
      (bir:replace-terminator jump (bir:end before))
      ;; The stuff in the AFTER block is now unreachable.
      (bir:delete-iblock after)
      (let ((call-arguments (rest (bir:inputs call)))
            (ftmd-arguments
              (loop for datum in (rest (bir:inputs call))
                    collect (make-instance 'bir:output
                              :name (bir:name datum)
                              :derived-type (bir:ctype datum))))
            (inputs '()))
        ;; Remove the local call.
        (bir:delete-instruction call)
        ;; Insert fixed-to-multiple instructions to ensure only primary values
        ;; of the call arguments are used.
        (loop for arg in call-arguments for ftm-out in ftmd-arguments
              for ftm = (make-instance 'bir:fixed-to-multiple
                          :inputs (list arg) :outputs (list ftm-out)
                          :origin origin :policy policy)
              do (bir:insert-instruction-before ftm jump))
        ;; Compute inputs to the jump.
        (bir:map-lambda-list
         (lambda (state item index)
           (declare (ignore index))
           (let ((arg (pop ftmd-arguments)))
             (ecase state
               (:required
                (push arg inputs))
               (&optional
                (let ((module (bir:module (bir:function call))))
                  (cond
                    (arg
                     (let* ((const (bir:constant-in-module t module))
                            (suppliedp-out (make-instance 'bir:output
                                             :derived-type (bir:ctype
                                                            (second item))))
                            (suppliedp (make-instance 'bir:constant-reference
                                         :inputs (list const)
                                         :outputs (list suppliedp-out)
                                         :origin origin :policy policy)))
                       (bir:insert-instruction-before suppliedp jump)
                       (push arg inputs)
                       (push suppliedp-out inputs)))
                    (t
                     (let* ((nil-constant
                              (bir:constant-in-module nil module))
                            (value-out (make-instance 'bir:output
                                         :derived-type (bir:ctype (first item))))
                            (value (make-instance
                                       'bir:constant-reference
                                     :inputs (list nil-constant)
                                     :outputs (list value-out)
                                     :origin origin :policy policy))
                            (suppliedp-out (make-instance 'bir:output
                                             :derived-type (bir:ctype
                                                            (second item))))
                            (suppliedp (make-instance 'bir:constant-reference
                                         :inputs (list nil-constant)
                                         :outputs (list suppliedp-out)
                                         :origin origin :policy policy)))
                       (bir:insert-instruction-before value jump)
                       (bir:insert-instruction-before suppliedp jump)
                       (push value-out inputs)
                       (push suppliedp-out inputs))))))
               (&rest
                ;; The argument is unused, so we don't need to pass anything.
                ;; To keep the BIR consistent, we need to outright delete any
                ;; LETI, because otherwise it would refer to a now-undefined
                ;; ARGUMENT.
                (assert (bir:unused-p item))
                (let ((use (bir:use item)))
                  (when (typep use 'bir:leti)
                    (bir:delete-instruction use)))))))
         lambda-list)
        (setf (bir:inputs jump) (nreverse inputs))))))

(defun rewire-return (function return-point-block)
  (let* ((returni (bir:returni function))
         (outputs (bir:inputs return-point-block))
         (jump (make-instance 'bir:jump
                 :inputs (if (bir:inputs return-point-block)
                             (bir:inputs returni)
                             '())
                 :outputs (copy-list outputs)
                 :next (list return-point-block)
                 :origin (bir:origin returni)
                 :policy (bir:policy returni))))
    ;; This assertion is sort of guaranteed by LOGICAL-CONTINUATION.
    (assert (<= (length (bir:inputs return-point-block))
                (length (bir:inputs returni))))
    (bir:replace-terminator jump returni)))

(defun move-function-arguments-to-iblock (function)
  (let ((start (bir:start function))
        (phis '()))
    (bir:map-lambda-list
     (lambda (state item index)
       (declare (ignore index))
       (ecase state
         (:required
          (let ((supplied (make-instance 'bir:phi
                            :iblock start
                            :derived-type (bir:ctype item))))
            (push supplied phis)
            (bir:replace-uses supplied item)))
         (&optional
          (let ((supplied (make-instance 'bir:phi
                            :iblock start
                            :derived-type (bir:ctype (first item))))
                (supplied-p (make-instance 'bir:phi
                              :iblock start
                              :derived-type (bir:ctype
                                             (second item)))))
            (push supplied phis)
            (push supplied-p phis)
            (bir:replace-uses supplied (first item))
            (bir:replace-uses supplied-p (second item))))
         (&rest
          ;; The argument is is unused, so don't do anything.
          (assert (bir:unused-p item)))))
     (bir:lambda-list function))
    (setf (bir:inputs start) (nreverse phis))))

;;; Integrate the blocks of FUNCTION into TARGET-FUNCTION in the
;;; dynamic environment DYNENV.
(defun interpolate-function (function target-function dynenv)
  (set:nunionf (bir:variables target-function) (bir:variables function))
  (set:nunionf (bir:catches target-function) (bir:catches function))
  ;; Re-home iblocks (and indirectly, instructions), and if the
  ;; function unwinds to its target function, change it to a local
  ;; unwind.
  (bir:do-iblocks (ib function)
    (let ((u (bir:end ib)))
      (when (typep u 'bir:unwind)
        (when (eq (bir:function (bir:destination u)) target-function)
          (replace-unwind u))))
    (when (eq (bir:dynamic-environment ib) function)
      (setf (bir:dynamic-environment ib)
            dynenv))
    (setf (bir:function ib) target-function))
  ;; FUNCTION no longer owns these blocks. Need to do this so triggers
  ;; don't accidentally clean them up.
  (setf (bir:start function) nil))

;;; This is essentially a filter, but since we only care about the single
;;; outside call case, we do it like this to avoid consing.
(defun unique-outside-call (function local-calls)
  (let ((result nil))
    (set:doset (call local-calls result)
      (unless (eq (bir:function call) function)
        (if result
            ;; This is the second outside call we've seen: fail
            (return nil)
            ;; This is the first outside call we've seen: record
            (setf result call))))))

;;; If there is a common return point, integrate FUNCTION into the
;;; graph of TARGET-OWNER and rewire the calls into the body of the
;;; FUNCTION.
;;; If the return continuation is unknown, it can still get contified
;;; as long as the local function never returns normally.
;;; When the function does return normally, wire the return value of
;;; the function into the common ``transitive'' use of the local calls.
(defun contify (function local-calls return-point common-use common-dynenv target-owner)
  (let* ((returni (bir:returni function))
         ;; If there is exactly one outside call to the function, it may be in
         ;; the middle of a block, and have its output used somewhere other
         ;; than a phi. In this situation we need to normalize the IR so that
         ;; the call is at the end of a block and passes to a phi.
         (unique-call
           (and (not (eq return-point :unknown))
                (unique-outside-call function local-calls)))
         (return-point
           (cond
             (unique-call
              (check-type unique-call bir:local-call)
              (let ((dummy-block
                      (nth-value 1 (bir:split-block-after unique-call)))
                    (ucall-out (bir:output unique-call)))
                (unless (bir:unused-p ucall-out)
                  (let ((phi (make-instance 'bir:phi
                               :iblock dummy-block
                               :derived-type (bir:ctype ucall-out))))
                    (setf (bir:inputs dummy-block) (list phi))
                    ;; Replace the call-as-datum with the return-values.
                    (bir:replace-uses phi ucall-out)))
                dummy-block))
             ((eq return-point :unknown) :unknown)
             ;; If there is more than one call, to share a continuation they
             ;; must all output to the same phi, so we don't need to normalize.
             (t (bir:iblock return-point))))
         (start (bir:start function)))
    (unless (and returni (eq return-point :unknown))
      (move-function-arguments-to-iblock function)
      (unless (eq return-point :unknown)
        (when returni
          (rewire-return function return-point)))
      (interpolate-function function
                            target-owner
                            common-dynenv)
      (set:doset (call local-calls) (rewire-call-into-body call start))
      ;; Recompute the flow order, as now the iblocks of the function
      ;; have been integrated into that of TARGET-OWNER.
      (bir:compute-iblock-flow-order target-owner)
      ;; Merge the blocks. Merge the tail first since the
      ;; interpolated function might just be one block.
      (when returni
        (bir:merge-successor-if-possible (bir:iblock returni)))
      (when unique-call
        (bir:merge-successor-if-possible (bir:iblock unique-call)))
      ;; We've interpolated and there are potentially useless
      ;; catches in TARGET-OWNER, so now that the IR is in a
      ;; consistent state, eliminate them.
      (eliminate-catches target-owner)
      t)))

;;; We can inline required, optional, and ignored &rest parameters.
(defun lambda-list-too-hairy-p (lambda-list)
  (let ((too-hairy-p nil))
    (bir:map-lambda-list
     (lambda (state item index)
       (declare (ignore index))
       (case state
         ((:required &optional))
         (&rest (setq too-hairy-p (not (bir:unused-p item))))
         (t (setq too-hairy-p t))))
     lambda-list)
    too-hairy-p))

(defun maybe-interpolate (function)
  ;; When a function has no enclose and returns to a single control
  ;; point, it is eligible for interpolation.
  (when (and (null (bir:enclose function))
             (not (lambda-list-too-hairy-p (bir:lambda-list function))))
    ;; FIXME: We should respect inline and not inline declarations.
    (let ((local-calls (bir:local-calls function)))
      (unless (or (set:empty-set-p local-calls)
                  (set:some (lambda (c) (typep c 'bir:mv-local-call))
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
