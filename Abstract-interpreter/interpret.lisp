(in-package #:cleavir-abstract-interpreter)

;;; TODO?: Move work flag into instructions, or otherwise ensure that flowing is
;;; non consing in itself.

(defvar *work*)

(defun mark (instruction) (setf (gethash instruction *work*) t))
(defun markedp (instruction) (values (gethash instruction *work*)))
(defun unmark (instruction) (remhash instruction *work*))

;;;

;;; Given info for the input to a function (its arguments in forward domains,
;;; or its return values in backward domains), flow and mark appropriately.
(defgeneric flow-call (domain function info))

(defun interpret-module (domains module)
  (let ((*work* (make-hash-table)))
    (bir:do-functions (function module)
      ;; If a function is not enclosed and has no local calls, it is the entry
      ;; point, and we must proceed as though it could be called externally.
      ;; If a function is enclosed, we proceed as though it could be called
      ;; externally. TODO: Mingle local call analysis so that we can be
      ;; a little smarter about that.
      (when (or (bir:enclose function)
                (set:empty-set-p (bir:local-calls function)))
        (loop for domain in domains
              do (flow-call domain function (supremum domain))))
      ;; Unconditionally interpret every instruction (forward, arbitrarily)
      (interpret-function-forward domains function (constantly t)))
    ;; Now iterate through every instruction repeatedly until we hit a fixpoint.
    (loop
      (bir:do-functions (function module)
        (when (zerop (hash-table-count *work*)) (return-from interpret-module))
        (interpret-function-backward domains function #'markedp)
        (when (zerop (hash-table-count *work*)) (return-from interpret-module))
        (interpret-function-forward domains function #'markedp)))))

(defun interpret-function-forward (domains function predicate)
  (bir:do-iblocks (ib function :forward)
    (bir:do-iblock-instructions (inst ib :forward)
      (maybe-interpret-instruction domains inst predicate))))

(defun interpret-function-backward (domains function predicate)
  (bir:do-iblocks (ib function :backward)
    (bir:do-iblock-instructions (inst ib :backward)
      (maybe-interpret-instruction domains inst predicate))))

(defgeneric interpret-instruction (domain instruction))

;;; When an instruction is marked, we try to flow all domains, not just the one
;;; that changed. Besides being simpler, this facilitates domains relying on
;;; each other's information to increase their precision.
(defun maybe-interpret-instruction (domains instruction predicate)
  (when (funcall predicate instruction)
    (unmark instruction)
    (loop for domain in domains
          do (interpret-instruction domain instruction)))
  (values))
