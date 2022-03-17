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

(defmethod interpret-module ((strategy sequential) domains (module bir:module))
  (let ((table (mark-table strategy)))
    (bir:do-functions (function module)
      ;; If a function is not enclosed and has no local calls, it is the entry
      ;; point, and we must proceed as though it could be called externally.
      ;; If a function is enclosed, we proceed as though it could be called
      ;; externally. TODO: Mingle local call analysis so that we can be
      ;; a little smarter about that.
      (when (or (bir:enclose function)
                (set:empty-set-p (bir:local-calls function)))
        (loop for domain in domains
              do (flow-call strategy domain function (supremum domain))))
      ;; Unconditionally interpret every instruction (forward, arbitrarily)
      (interpret-function-forward table domains function (constantly t)))
    ;; Now iterate through every instruction repeatedly until we hit a fixpoint.
    (flet ((markedp (instruction)
             (values (gethash instruction table))))
      (loop
        (bir:do-functions (function module)
          (when (zerop (hash-table-count table)) (return-from interpret-module))
          (interpret-function-backward strategy domains function #'markedp)
          (when (zerop (hash-table-count table)) (return-from interpret-module))
          (interpret-function-forward strategy domains function #'markedp))))))

(defun interpret-function-forward (strategy domains function predicate)
  (bir:do-iblocks (ib function :forward)
    (bir:do-iblock-instructions (inst ib :forward)
      (maybe-interpret-instruction strategy domains inst predicate))))

(defun interpret-function-backward (strategy domains function predicate)
  (bir:do-iblocks (ib function :backward)
    (bir:do-iblock-instructions (inst ib :backward)
      (maybe-interpret-instruction strategy domains inst predicate))))

(defun maybe-interpret-instruction (strategy domains instruction predicate)
  (when (funcall predicate instruction)
    (unmark strategy instruction)
    (loop for domain in domains
          do (interpret-instruction strategy domain instruction)))
  (values))
