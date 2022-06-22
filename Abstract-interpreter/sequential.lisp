(in-package #:cleavir-abstract-interpreter)

;;;; The SEQUENTIAL strategy proceeds sequentially: it interprets every
;;;; instruction once, moving in forward flow order, then continues in backward
;;;; then forward flow order again and again until no new information is made
;;;; available.

;;; TODO?: Move work flag into instructions, or otherwise ensure that flowing is
;;; non consing in itself.
(defclass sequential (strategy)
  ((%mark-table :initform (make-hash-table) :reader mark-table)))

(defmethod mark ((strategy sequential) instruction)
  (setf (gethash instruction (mark-table strategy)) t))
(defun unmark (strategy instruction)
  (remhash instruction (mark-table strategy)))

;;;

(defmethod interpret-module ((strategy sequential) (product product) (module bir:module))
  ;; Initialize all infos and entry points.
  (let ((domains (domains product)))
    (bir:do-functions (function module)
      (bir:map-local-instructions
       (lambda (inst)
         (dolist (domain domains)
           (initialize-instruction strategy domain inst)))
       function))
    ;; Entry points. FIXME: BIR should indicate entry points better.
    (bir:do-functions (function module)
      (when (or (bir:enclose function) (set:empty-set-p (bir:local-calls function)))
        (dolist (domain domains)
          (initialize-entry-point strategy domain function)))))
  (bir:do-functions (function module)
    ;; Unconditionally interpret every instruction (forward, arbitrarily)
    (interpret-function-forward strategy product function (constantly t)))
  ;; Now iterate through every instruction repeatedly until we hit a fixpoint.
  (let ((table (mark-table strategy)))
    (flet ((markedp (instruction)
             (values (gethash instruction table))))
      (loop
        (bir:do-functions (function module)
          (when (zerop (hash-table-count table)) (return-from interpret-module))
          (interpret-function-backward strategy product function #'markedp)
          (when (zerop (hash-table-count table)) (return-from interpret-module))
          (interpret-function-forward strategy product function #'markedp))))))

(defun interpret-function-forward (strategy product function predicate)
  (bir:do-iblocks (ib function :forward)
    (bir:do-iblock-instructions (inst ib :forward)
      (maybe-interpret-instruction strategy product inst predicate))))

(defun interpret-function-backward (strategy product function predicate)
  (bir:do-iblocks (ib function :backward)
    (bir:do-iblock-instructions (inst ib :backward)
      (maybe-interpret-instruction strategy product inst predicate))))

(defun maybe-interpret-instruction (strategy product instruction predicate)
  (when (funcall predicate instruction)
    (unmark strategy instruction)
    (interpret-instruction strategy product instruction))
  (values))
