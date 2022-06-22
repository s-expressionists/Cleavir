(in-package #:cleavir-abstract-interpreter)

;;;; Forward and backward data flows.
;;;; More formally, these are Cartesian domains.

(defmethod initialize-entry-point ((strategy strategy) (domain forward-data)
                                   (function bir:function))
  ;; Make the arguments supreme.
  (let ((sup (supremum domain)))
    (bir:map-lambda-list
     (lambda (state item index)
       (declare (ignore index))
       (ecase state
         ((:required &rest)
          (setf (info strategy domain item) sup))
         ((&optional)
          (setf (info strategy domain (first item)) sup
                (info strategy domain (second item)) sup))
         ((&key)
          (setf (info strategy domain (second item)) sup
                (info strategy domain (third item)) sup))))
     (bir:lambda-list function))))

(defmethod initialize-instruction ((strategy optimism) (domain forward-data)
                                   (inst bir:instruction))
  (loop with inf = (infimum domain)
        for out in (bir:outputs inst)
        do (setf (info strategy domain out) inf)))
(defmethod initialize-instruction ((strategy pessimism) (domain forward-data)
                                   (inst bir:instruction))
  (loop with sup = (supremum domain)
        for out in (bir:outputs inst)
        do (setf (info strategy domain out) sup)))

(defmethod initialize-entry-point ((strategy strategy) (domain backward-data)
                                   (function bir:function))
  (let ((ret (bir:returni function)))
    (when ret
      (setf (info strategy domain (bir:input ret)) (supremum domain)))))

(defmethod initialize-instruction ((strategy optimism) (domain backward-data)
                                   (inst bir:instruction))
  (loop with inf = (infimum domain)
        for inp in (bir:inputs inst)
        do (setf (info strategy domain inp) inf)))
(defmethod initialize-instruction ((strategy pessimism) (domain backward-data)
                                   (inst bir:instruction))
  (loop with sup = (supremum domain)
        for inp in (bir:inputs inst)
        do (setf (info strategy domain inp) sup)))

;;; Given new info for a datum, maybe store it and mark if it's novel.
(defgeneric maybe-mark-datum (strategy domain datum info))
;;; Mark the uses of a datum.
(defgeneric mark-datum-through (strategy domain datum))
;;; Given that DATUM is an input to a local call, maybe mark appropriately.
(defgeneric mark-through-local-call (strategy domain datum local-call))

(defmethod maybe-mark-datum ((strategy optimism) (domain domain) (datum bir:datum)
                             info)
  (let* ((old (info strategy domain datum))
         (new (join domain old info)))
    ;; We check for a definite not-subinfo instead of just not subinfo
    ;; so that, if the relation can't be determined, we don't just keep
    ;; repeating.
    (multiple-value-bind (sub surety) (subinfop domain new old)
      (when (and surety (not sub))
        (setf (info strategy domain datum) new)
        (mark-datum-through strategy domain datum)))))

(defmethod maybe-mark-datum ((strategy pessimism) (domain domain) (datum bir:datum)
                             info)
  (let* ((old (info strategy domain datum))
         (new (meet domain old info)))
    (multiple-value-bind (sub surety) (subinfop domain old new)
      (when (and surety (not sub))
        (setf (info strategy domain datum) new)
        (mark-datum-through strategy domain datum)))))

(defmethod maybe-mark-datum ((strategy pessimism) (domain domain) (datum bir:phi)
                             info)
  (declare (ignore info))
  (let* ((old (info strategy domain datum))
         (new (infimum domain)))
    (set:doset (inp (bir:phi-inputs datum))
      ;; In the way we construct BIR, it's not possible for a phi to be
      ;; dependent on itself. As such we should not need to widen.
      (setf new (join domain new (info strategy domain inp))))
    (setf new (meet domain old new))
    (multiple-value-bind (sub surety) (subinfop domain old new)
      (when (and surety (not sub))
        (setf (info strategy domain datum) new)
        (mark-datum-through strategy domain datum)))))

(defmethod maybe-mark-datum ((strategy optimism) (domain domain) (datum bir:variable)
                             info)
  (let* ((old (info strategy domain datum))
         ;; widening to ensure termination, since variables can be in loops
         (new (widen domain old (join domain old info))))
    (multiple-value-bind (sub surety) (subinfop domain new old)
      (when (and surety (not sub))
        (setf (info strategy domain datum) new)
        (mark-datum-through strategy domain datum)))))

(defmethod maybe-mark-datum ((strategy pessimism) (domain forward-data)
                             (datum bir:variable) info)
  (declare (ignore info))
  (let* ((old (info strategy domain datum))
         (new (infimum domain)))
    (set:doset (writer (bir:writers datum))
      (let ((info (info strategy domain (bir:input writer))))
        (setf new (join domain info new))))
    (setf new (meet domain old new))
    (multiple-value-bind (sub surety) (subinfop domain old new)
      (when (and surety (not sub))
        (setf (info strategy domain datum) new)
        (mark-datum-through strategy domain datum)))))

(defmethod maybe-mark-datum ((strategy pessimism) (domain backward-data)
                             (datum bir:variable) info)
  (declare (ignore info))
  (let* ((old (info strategy domain datum))
         (new (infimum domain)))
    (set:doset (reader (bir:readers datum))
      (let ((info (info strategy domain (bir:input reader))))
        (setf new (join domain info new))))
    (setf new (meet domain old new))
    (multiple-value-bind (sub surety) (subinfop domain old new)
      (when (and surety (not sub))
        (setf (info strategy domain datum) new)
        (mark-datum-through strategy domain datum)))))

(defmethod mark-datum-through ((strategy strategy) (domain forward-data)
                               (datum bir:linear-datum))
  (let ((use (bir:use datum)))
    (when use
      (if (typep use 'bir:abstract-local-call)
          ;; If our use is a local call, we need to mark the
          ;; corresponding argument instead.
          (mark-through-local-call strategy domain datum use)
          (mark strategy use)))))

(defmethod mark-through-local-call ((strategy strategy) (domain forward-data)
                                    (datum bir:linear-datum) (call bir:abstract-local-call))
  ;; FIXME: Propagate infos to arguments.
  (mark strategy (bir:start (bir:start (bir:callee call)))))

(defmethod mark-through-local-call ((strategy strategy) (domain backward-data)
                                    (datum bir:linear-datum) (call bir:abstract-local-call))
  (let ((ret (bir:returni (bir:callee call))))
    (when ret
      (mark strategy ret))))

(defmethod mark-datum-through ((strategy strategy) (domain forward-data)
                               (datum bir:variable))
  (set:doset (reader (bir:readers datum)) (mark strategy reader)))

(defmethod mark-datum-through ((strategy strategy) (domain backward-data)
                               (datum bir:output))
  (mark strategy (bir:definition datum)))

(defmethod mark-datum-through ((strategy strategy) (domain backward-data)
                               (datum bir:phi))
  (set:doset (def (bir:definitions datum)) (mark strategy def)))

(defmethod mark-datum-through ((strategy strategy) (domain backward-data)
                               (datum bir:variable))
  (set:doset (writer (bir:writers datum)) (mark strategy writer)))

(defmethod mark-datum-through ((strategy strategy) (domain backward-data)
                               (datum bir:value)))

;;;

(defmethod instruction-input-info ((strategy strategy) (domain forward-data)
                                   (instruction bir:instruction))
  (values-list
   (loop for inp in (bir:inputs instruction)
         collect (info strategy domain inp))))
(defmethod instruction-input-info ((strategy strategy) (domain forward-data)
                                   (instruction bir:one-input))
  (info strategy domain (bir:input instruction)))
(defmethod instruction-input-info ((strategy strategy) (domain forward-data)
                                   (instruction bir:no-input))
  (values))

(defmethod instruction-input-info ((strategy strategy) (domain backward-data)
                                   (instruction bir:instruction))
  (values-list
   (loop for out in (bir:outputs instruction)
         collect (info strategy domain out))))
(defmethod instruction-input-info ((strategy strategy) (domain backward-data)
                                   (instruction bir:one-output))
  (info strategy domain (bir:output instruction)))
(defmethod instruction-input-info ((strategy strategy) (domain backward-data)
                                   (instruction bir:no-output))
  (values))
(defmethod instruction-input-info ((strategy strategy) (domain backward-data)
                                   (inst bir:returni))
  (let* ((function (bir:function inst))
          (local-calls (bir:local-calls function)))
     (if (or (bir:enclose function) (set:empty-set-p local-calls))
         ;; arbitrary call sites, give up
         (supremum domain)
         (let ((result (infimum domain)))
           (set:doset (call local-calls result)
             (setf result (join result
                                (info strategy domain (bir:output call)))))))))

;;;

(defmethod instruction-output-info ((strategy strategy) (domain forward-data)
                                    (inst bir:instruction) &rest out-infos)
  (loop for output in (bir:outputs inst)
        for out-info in out-infos
        do (maybe-mark-datum strategy domain output out-info)))
(defmethod instruction-output-info ((strategy strategy) (domain forward-data)
                                    (inst bir:one-output) &rest out-infos)
  (maybe-mark-datum strategy domain (bir:output inst) (first out-infos)))
(defmethod instruction-output-info ((strategy strategy) (domain forward-data)
                                    (inst bir:no-output) &rest out-infos)
  (declare (ignore out-infos)))
(defmethod instruction-output-info ((strategy strategy) (domain forward-data)
                                    (inst bir:returni) &rest out-infos)
  (let ((info (first out-infos)))
    (set:doset (call (bir:local-calls (bir:function inst)))
      (maybe-mark-datum strategy domain (bir:output call) info))))

(defmethod instruction-output-info ((strategy strategy) (domain backward-data)
                                    (inst bir:instruction) &rest in-infos)
  (loop for input in (bir:inputs inst)
        for in-info in in-infos
        do (maybe-mark-datum strategy domain input in-info)))
(defmethod instruction-output-info ((strategy strategy) (domain backward-data)
                                    (inst bir:one-input) &rest in-infos)
  (maybe-mark-datum strategy domain (bir:input inst) (first in-infos)))
(defmethod instruction-output-info ((strategy strategy) (domain backward-data)
                                    (inst bir:no-input) &rest in-infos)
  (declare (ignore in-infos)))

;;;

(defmethod flow-instruction ((domain forward-data) (inst bir:instruction)
                             &rest in-infos)
  (declare (ignore in-infos))
  (values-list (make-list (length (bir:outputs inst))
                          :initial-element (supremum domain))))
(defmethod flow-instruction ((domain forward-data) (inst bir:one-output)
                             &rest in-infos)
  (declare (ignore in-infos))
  (supremum domain))
(defmethod flow-instruction ((domain forward-data) (inst bir:no-output)
                             &rest in-infos)
  (declare (ignore in-infos))
  (values))

;;;

(defmethod flow-instruction ((domain backward-data) (instruction bir:instruction)
                             &rest out-infos)
  (declare (ignore out-infos))
  (values-list (make-list (length (bir:inputs instruction))
                          :initial-element (supremum domain))))

(defmethod flow-instruction ((domain forward-data) (instruction bir:values-save)
                             &rest in-infos)
  (first in-infos))

(defmethod flow-instruction ((domain backward-data) (instruction bir:values-save)
                             &rest out-infos)
  (first out-infos))

(defmethod flow-instruction ((domain forward-data) (instruction bir:values-restore)
                             &rest in-infos)
  (first in-infos))

(defmethod flow-instruction ((domain backward-data) (instruction bir:values-restore)
                             &rest out-infos)
  (first out-infos))

;;; forward writevar and backward readvar need values info, but we can do this
(defmethod flow-instruction ((domain forward-data) (instruction bir:readvar)
                             &rest in-infos)
  (first in-infos))
(defmethod flow-instruction ((domain backward-data) (instruction bir:writevar)
                             &rest out-infos)
  (first out-infos))

(defmethod flow-instruction ((domain forward-data) (inst bir:jump) &rest in-infos)
  (values-list in-infos))

(defmethod flow-instruction ((domain backward-data) (inst bir:jump) &rest out-infos)
  (values-list out-infos))

(defmethod flow-instruction ((domain forward-data) (inst bir:unwind) &rest in-infos)
  (values-list in-infos))

(defmethod flow-instruction ((domain backward-data) (inst bir:unwind) &rest out-infos)
  (values-list out-infos))
