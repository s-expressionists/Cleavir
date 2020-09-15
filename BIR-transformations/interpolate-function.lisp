(in-package #:cleavir-bir-transformations)

;;; interpolated-function must have only required parameters
(defun interpolate-function (interpolated-function call)
  (let* ((lambda-list (cleavir-bir:lambda-list interpolated-function))
         (call-block (cleavir-bir:iblock call))
         (call-function (cleavir-bir:function call-block))
         (interp-end (cleavir-bir:end interpolated-function))
         (returni (when interp-end (cleavir-bir:end interp-end)))
         (return-values (when interp-end (first (cleavir-bir:inputs returni))))
         (enclose (first (cleavir-bir:inputs call)))
         (arguments (rest (cleavir-bir:inputs call))))
    (check-type enclose cleavir-bir:enclose)
    (assert (every (lambda (a) (typep a 'cleavir-bir:argument)) lambda-list))
    ;; Rewire control
    (multiple-value-bind (before after)
        (cleavir-bir:split-block-after call)
      ;; BEFORE is now a block that jumps with no arguments to AFTER.
      ;; Change it to a leti into the interpolated function's start block.
      (let ((leti (make-instance 'cleavir-bir:leti
                    :iblock before
                    :next (list (cleavir-bir:start interpolated-function)))))
        (cleavir-bir:replace-terminator
         leti
         (cleavir-bir:end before))
        (setf (cleavir-bir:bindings leti)
              (cleavir-set:filter
               'cleavir-set:set
               (lambda (v)
                 (when (eq (cleavir-bir:binder v) interpolated-function)
                   (setf (cleavir-bir:binder v) leti)
                   (cleavir-set:nadjoinf (cleavir-bir:bindings leti) v)
                   t))
               (cleavir-bir:variables interpolated-function)))
        ;; Replace the return instruction with a jump if there is one, or else
        ;; delete AFTER and any later straight line blocks.
        (if interp-end
            (cleavir-bir:replace-terminator
             (make-instance 'cleavir-bir:jump
               :unwindp t :inputs () :outputs ()
               :iblock interp-end :next (list after))
             returni)
            (cleavir-bir:delete-block after))
        ;; If the interpolated function unwinds to the call function, change it
        ;; to a local unwind.
        (cleavir-set:doset (ib (cleavir-bir:exits interpolated-function))
          (let ((u (cleavir-bir:end ib)))
            (check-type u cleavir-bir:unwind)
            (let ((dest (cleavir-bir:destination u)))
              (when (eq (cleavir-bir:function dest) call-function)
                (let ((new (make-instance 'cleavir-bir:jump
                             :inputs (rest (cleavir-bir:inputs u))
                             :outputs (cleavir-bir:outputs u)
                             :unwindp t :next (list dest))))
                  (cleavir-bir:replace-terminator new u)
                  (cleavir-bir:move-inputs new))))))
        (when return-values
          ;; Replace the call-as-datum with the return-values.
          (cleavir-bir:replace-computation call return-values))
        ;; Replace the arguments in the interpolated function body with the
        ;; actual argument values
        (mapc #'cleavir-bir:replace-linear-datum arguments lambda-list)
        ;; Delete the enclose.
        (cleavir-bir:delete-computation enclose)
        ;; Re-home iblocks (and indirectly, instructions)
        (cleavir-bir:map-iblocks
         (lambda (ib)
           (when (eq (cleavir-bir:dynamic-environment ib) interpolated-function)
             (setf (cleavir-bir:dynamic-environment ib) leti))
           (setf (cleavir-bir:function ib) call-function))
         interpolated-function)))
    ;; Re-home variables
    (cleavir-set:doset (v (cleavir-bir:variables interpolated-function))
      (cond (;; If the interpolated function owns the variable just update it.
             ;; Note that this will include local variables.
             (eq (cleavir-bir:owner v) interpolated-function)
             (setf (cleavir-bir:owner v) call-function)
             (cleavir-set:nadjoinf (cleavir-bir:variables call-function) v))
            ((eq (cleavir-bir:owner v) call-function)
             ;; If the variable is owned by the function being interpolated
             ;; into, it's possible that it was only shared between these two
             ;; functions, and so is now local.
             (flet ((owned-by-call-function-p (inst)
                      (eq (cleavir-bir:function inst) call-function)))
               (when (and (cleavir-set:empty-set-p (cleavir-bir:encloses v))
                          (cleavir-set:every #'owned-by-call-function-p
                                             (cleavir-bir:readers v))
                          (cleavir-set:every #'owned-by-call-function-p
                                             (cleavir-bir:writers v)))
                 (setf (cleavir-bir:extent v) :local))))
            ;; If it's owned by some higher function, don't touch it
            (t))))
  (values))
