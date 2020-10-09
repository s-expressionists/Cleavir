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
      ;; BEFORE is now a block that jumps with no arguments to
      ;; AFTER. Insert a leti into the interpolated function's start
      ;; block to represent the binding action of the function.
      (let ((leti (make-instance 'cleavir-bir:leti)))
        (cleavir-bir:insert-instruction-before
         leti
         (cleavir-bir:start (cleavir-bir:start interpolated-function)))
        (cleavir-bir:replace-terminator
         (make-instance 'cleavir-bir:jump
                        :next (list (cleavir-bir:start interpolated-function))
                        :inputs () :outputs ())
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
        (cleavir-set:nunionf (cleavir-bir:variables call-function)
                             (cleavir-bir:variables interpolated-function))
        ;; Replace the return instruction with a jump if there is one, or else
        ;; delete AFTER and any later straight line blocks.
        (if interp-end
            (cleavir-bir:replace-terminator
             (make-instance 'cleavir-bir:jump
               :unwindp t :inputs () :outputs ()
               :next (list after))
             returni)
            (cleavir-bir:delete-iblock after))
        ;; If the interpolated function unwinds to the call function, change it
        ;; to a local unwind.
        (cleavir-set:doset (ib (cleavir-bir:exits interpolated-function))
          (let ((u (cleavir-bir:end ib)))
            (check-type u cleavir-bir:unwind)
            (let ((dest (cleavir-bir:destination u)))
              (when (eq (cleavir-bir:function dest) call-function)
                (let ((contread (first (cleavir-bir:inputs u)))
                      (unwind-inputs (rest (cleavir-bir:inputs u)))
                      (unwind-outputs (cleavir-bir:outputs u))
                      (new (make-instance 'cleavir-bir:jump
                             :unwindp t :next (list dest))))
                  (cleavir-bir:replace-terminator new u)
                  (cleavir-bir:delete-computation contread)
                  (setf (cleavir-bir:inputs new) unwind-inputs
                        (cleavir-bir:outputs new) unwind-outputs))))))
        (cond
          (return-values
           (let ((call-use (unless (cleavir-bir:unused-p call)
                             (cleavir-bir:use call))))
             ;; Replace the call-as-datum with the return-values.
             (cleavir-bir:replace-computation call return-values)
             ;; If we have a FTM->MTF sequence, clear it out.
             (when (and call-use
                        (typep return-values 'cleavir-bir:fixed-to-multiple)
                        (typep call-use 'cleavir-bir:multiple-to-fixed))
               (cleavir-bir:delete-transmission return-values call-use))))
          (t
           ;; The call isn't used, so simply delete it.
           (cleavir-bir:delete-computation call)))
        ;; Replace the arguments in the interpolated function body with the
        ;; actual argument values
        (mapc #'cleavir-bir:replace-uses arguments lambda-list)
        ;; Delete the enclose.
        (cleavir-bir:delete-computation enclose)
        ;; Remove the function from the module.
        (cleavir-set:nremovef (cleavir-bir:functions (cleavir-bir:module call-function))
                              interpolated-function)
        ;; Re-home iblocks (and indirectly, instructions)
        (cleavir-bir:map-iblocks
         (lambda (ib)
           (when (eq (cleavir-bir:dynamic-environment ib) interpolated-function)
             (setf (cleavir-bir:dynamic-environment ib)
                   (cleavir-bir:dynamic-environment before)))
           (setf (cleavir-bir:function ib) call-function))
         interpolated-function)
        ;; Merge the blocks. Merge the tail first since the
        ;; interpolated function might just be one block.
        (when (and interp-end
                   ;; FIXME: because tagbody is a little funky right
                   ;; now, we workaround the fact that sometimes
                   ;; interp-end exists but is sitll unreachable by
                   ;; its functions, meaning its function block never
                   ;; gets updated. Remove this clause ASAP as soon as
                   ;; AST-to-BIR is fixed.
                   (eq (cleavir-bir:function interp-end)
                       (cleavir-bir:function after)))
          (cleavir-bir:merge-iblocks interp-end after))
        (cleavir-bir:merge-iblocks before (cleavir-bir:start interpolated-function)))))
  (values))
