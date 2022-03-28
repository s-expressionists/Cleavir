(in-package #:cleavir-abstract-interpreter)

;;;; Forward and backward data flows.

;;; Call FLOW-DATUM-THROUGH when the existing info is not a subinfo of the
;;; provided new info.
(defgeneric flow-datum (strategy domain datum info))
;;; Mark instructions related to the data for further flow.
(defgeneric flow-datum-through (strategy domain datum))

(defmethod flow-datum ((strategy optimism) (domain domain) (datum bir:datum)
                       info)
  (let* ((old (info strategy domain datum))
         (new (join domain old info)))
    ;; We check for a definite not-subinfo instead of just not subinfo
    ;; so that, if the relation can't be determined, we don't just keep
    ;; repeating.
    (multiple-value-bind (sub surety) (subinfop domain new old)
      (when (and surety (not sub))
        (setf (info strategy domain datum) new)
        (flow-datum-through strategy domain datum)))))

(defmethod flow-datum ((strategy pessimism) (domain domain) (datum bir:datum)
                       info)
  (let* ((old (info strategy domain datum))
         (new (meet domain old info)))
    (multiple-value-bind (sub surety) (subinfop domain old new)
      (when (and surety (not sub))
        (setf (info strategy domain datum) new)
        (flow-datum-through strategy domain datum)))))

(defmethod flow-datum ((strategy pessimism) (domain domain) (datum bir:phi)
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
        (flow-datum-through strategy domain datum)))))

(defmethod flow-datum ((strategy optimism) (domain domain) (datum bir:variable)
                       info)
  (let* ((old (info strategy domain datum))
         ;; widening join to ensure termination
         (new (wjoin domain old info)))
    (multiple-value-bind (sub surety) (subinfop domain new old)
      (when (and surety (not sub))
        (setf (info strategy domain datum) new)
        (flow-datum-through strategy domain datum)))))

(defmethod flow-datum ((strategy pessimism) (domain forward-data)
                       (datum bir:variable) info)
  (declare (ignore info))
  (let* ((old (info strategy domain datum))
         (new (infimum domain)))
    (set:doset (writer (bir:writers datum))
      (let ((in (bir:input writer)))
        (setf new (wjoin domain new (info strategy domain in)))))
    (setf new (meet domain old new))
    (multiple-value-bind (sub surety) (subinfop domain old new)
      (when (and surety (not sub))
        (setf (info strategy domain datum) new)
        (flow-datum-through strategy domain datum)))))

(defmethod flow-datum ((strategy pessimism) (domain backward-data)
                       (datum bir:variable) info)
  (declare (ignore info))
  (let* ((old (info strategy domain datum))
         (new (infimum domain)))
    (set:doset (reader (bir:readers datum))
      (let ((in (bir:input reader)))
        (setf new (wjoin domain new (info strategy domain in)))))
    (setf new (meet domain old new))
    (multiple-value-bind (sub surety) (subinfop domain old new)
      (when (and surety (not sub))
        (setf (info strategy domain datum) new)
        (flow-datum-through strategy domain datum)))))

(defmethod flow-datum-through ((strategy strategy) (domain forward-data)
                               (datum bir:linear-datum))
  (let ((use (bir:use datum)))
    (when use (mark strategy use))))

(defmethod flow-datum-through ((strategy strategy) (domain forward-data)
                               (datum bir:variable))
  (set:doset (reader (bir:readers datum)) (mark strategy reader)))

(defmethod flow-datum-through ((strategy strategy) (domain backward-data)
                               (datum bir:output))
  (mark strategy (bir:definition datum)))

  (defmethod flow-datum-through ((strategy strategy) (domain backward-data)
                                 (datum bir:phi))
  (set:doset (def (bir:definitions datum)) (mark strategy def)))

(defmethod flow-datum-through ((strategy strategy) (domain backward-data)
                               (datum bir:variable))
  (set:doset (writer (bir:writers datum)) (mark strategy writer)))

(defmethod flow-datum-through ((strategy strategy) (domain backward-data)
                               (datum bir:value)))

;;; We mark calls regardless of direction because if we were to track what
;;; calls encloses, anything interpreted backwards through the arguments would
;;; still need to result in forward propagation to calls to the function.
(defmethod flow-datum-through ((strategy strategy) (domain data)
                               (datum bir:function))
  (set:doset (call (bir:local-calls datum)) (mark strategy call))
  (let ((enclose (bir:enclose datum)))
    (when enclose (mark strategy enclose))))

;;; TODO
(defmethod flow-datum-through ((strategy strategy) (domain backward-data)
                               (datum bir:argument)))

;;;

(defmethod interpret-instruction ((strategy strategy) (domain forward-data)
                                  (product product)
                                  (instruction bir:instruction))
  (loop with sup = (supremum domain)
        for output in (bir:outputs instruction)
        do (flow-datum strategy domain output sup)))

(defmethod interpret-instruction :around ((strategy strategy)
                                          (domain forward-data)
                                          (product product)
                                          (instruction bir:instruction))
  ;; If an instruction is not reachable, its outputs must all be the infimum.
  (let* ((reachability (reachability product))
         (reachablep (if reachability
                         (info strategy reachability instruction)
                         t)))
    (if reachablep
        (call-next-method)
        (loop with inf = (infimum domain)
              for output in (bir:outputs instruction)
              do (flow-datum strategy domain output inf)))))

(defmethod interpret-instruction ((strategy strategy) (domain backward-data)
                                  (product product)
                                  (instruction bir:instruction))
  (loop with sup = (supremum domain)
        for input in (bir:inputs instruction)
        do (flow-datum strategy domain input sup)))

(defmethod interpret-instruction :around ((strategy strategy)
                                          (domain backward-data)
                                          (product product)
                                          (instruction bir:instruction))
  (let* ((reachability (reachability product))
         (reachablep (info strategy reachability instruction)))
    (if reachablep
        (call-next-method)
        (loop with inf = (infimum domain)
              for input in (bir:inputs instruction)
              do (flow-datum strategy domain input inf)))))

(defmethod interpret-instruction ((strategy strategy) (domain forward-data)
                                  (product product)
                                  (instruction bir:values-save))
  (flow-datum strategy domain (bir:output instruction)
              (info strategy domain (bir:input instruction))))

(defmethod interpret-instruction ((strategy strategy) (domain backward-data)
                                  (product product)
                                  (instruction bir:values-save))
  (flow-datum strategy domain (bir:input instruction)
              (info strategy domain (bir:output instruction))))

(defmethod interpret-instruction ((strategy strategy) (domain forward-data)
                                  (product product)
                                  (instruction bir:values-restore))
  (flow-datum strategy domain (bir:output instruction)
              (info strategy domain (bir:input instruction))))

(defmethod interpret-instruction ((strategy strategy) (domain backward-data)
                                  (product product)
                                  (instruction bir:values-restore))
  (flow-datum strategy domain (bir:input instruction)
              (info strategy domain (bir:output instruction))))

(defmethod interpret-instruction ((strategy strategy) (domain forward-data)
                                  (product product)
                                  (inst bir:jump))
  (loop for inp in (bir:inputs inst)
        for info = (info strategy domain inp)
        for oup in (bir:outputs inst)
        do (flow-datum strategy domain oup info)))

(defmethod interpret-instruction ((strategy strategy) (domain backward-data)
                                  (product product)
                                  (inst bir:jump))
  (loop for inp in (bir:inputs inst)
        for oup in (bir:outputs inst)
        for info = (info strategy domain oup)
        do (flow-datum strategy domain inp info)))

(defmethod interpret-instruction ((strategy strategy) (domain forward-data)
                                  (product product)
                                  (inst bir:unwind))
  (loop for inp in (bir:inputs inst)
        for info = (info strategy domain inp)
        for oup in (bir:outputs inst)
        do (flow-datum strategy domain oup info)))

(defmethod interpret-instruction ((strategy strategy) (domain backward-data)
                                  (product product)
                                  (inst bir:unwind))
  (loop for inp in (bir:inputs inst)
        for oup in (bir:outputs inst)
        for info = (info strategy domain oup)
        do (flow-datum strategy domain inp info)))

;;; functions:
;;; We associate domain information with each function. This represents the
;;; info for the return value for forward data, and for the arguments for
;;; backward data. The info is essentially averaged, in that for example for
;;; forward data, the eventual fixed point of the function output info will be
;;; the join of that obtainable by abstract interpretation from all call sites.
;;; This could be refined (FIXME).

(defmethod interpret-instruction ((strategy strategy) (domain forward-data)
                                  (product product)
                                  (inst bir:returni))
  (flow-datum strategy domain (bir:function inst)
              (info strategy domain (bir:input inst))))

(defmethod interpret-instruction ((strategy strategy) (domain backward-data)
                                  (product product)
                                  (inst bir:returni))
  (flow-datum strategy domain (bir:input inst)
              (info strategy domain (bir:function inst))))

;;; For local-call we need values, so that's in values-data, but for mv calls
;;; we can proceed simply.
(defmethod interpret-instruction ((strategy strategy) (domain forward-data)
                                  (product product)
                                  (inst bir:mv-local-call))
  (let ((callee (bir:callee inst)))
    (flow-call strategy domain callee
               (info strategy domain (second (bir:inputs inst))))
    (flow-datum strategy domain (bir:output inst)
                (info strategy domain callee))))

(defmethod flow-call ((strategy strategy) (domain backward-data)
                      (function bir:function) info)
  (flow-datum strategy domain function info)
  (let ((returni (bir:returni function)))
    (when returni (mark strategy returni))))

(defmethod interpret-instruction ((strategy strategy) (domain backward-data)
                                  (product product)
                                  (inst bir:mv-local-call))
  (let ((callee (bir:callee inst)))
    (flow-call strategy domain callee (info strategy domain (bir:output inst)))
    (flow-datum strategy domain (second (bir:inputs inst))
                (info strategy domain callee))))
