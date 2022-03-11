(in-package #:cleavir-abstract-interpreter)

;;;; Forward and backward data flows.

;;; Call FLOW-DATUM-THROUGH when the existing info is not a subinfo of the
;;; provided new info.
(defgeneric flow-datum (domain datum info))
;;; Mark instructions related to the data for further flow.
(defgeneric flow-datum-through (domain datum))

(defmethod flow-datum ((domain domain) (datum bir:datum) info)
  (let* ((old (info domain datum))
         (new (join domain old info)))
    ;; We check for a definite not-subinfo instead of just not subinfo
    ;; so that, if the relation can't be determined, we don't just keep
    ;; repeating.
    (multiple-value-bind (sub surety) (subinfop domain new old)
      (when (and surety (not sub))
        (setf (info domain datum) new)
        (flow-datum-through domain datum)))))

(defmethod flow-datum ((domain domain) (datum bir:variable) info)
  (let* ((old (info domain datum))
         ;; widening join to ensure termination
         (new (wjoin domain old info)))
    (multiple-value-bind (sub surety) (subinfop domain new old)
      (when (and surety (not sub))
        (setf (info domain datum) new)
        (flow-datum-through domain datum)))))

(defmethod flow-datum-through ((domain forward-data) (datum bir:linear-datum))
  (let ((use (bir:use datum)))
    (when use (mark use))))

(defmethod flow-datum-through ((domain forward-data) (datum bir:variable))
  (set:mapset nil #'mark (bir:readers datum)))

(defmethod flow-datum-through ((domain backward-data) (datum bir:output))
  (mark (bir:definition datum)))

(defmethod flow-datum-through ((domain backward-data) (datum bir:phi))
  (set:mapset nil #'mark (bir:definitions datum)))

(defmethod flow-datum-through ((domain backward-data) (datum bir:variable))
  (set:mapset nil #'mark (bir:writers datum)))

(defmethod flow-datum-through ((domain backward-data) (datum bir:value)))

;;; We mark calls regardless of direction because if we were to track what
;;; calls encloses, anything interpreted backwards through the arguments would
;;; still need to result in forward propagation to calls to the function.
(defmethod flow-datum-through ((domain data) (datum bir:function))
  (set:mapset nil #'mark (bir:local-calls datum))
  (let ((enclose (bir:enclose datum)))
    (when enclose (mark enclose))))

;;; TODO
(defmethod flow-datum-through ((domain backward-data) (datum bir:argument)))

;;;

(defmethod interpret-instruction ((domain forward-data)
                                  (instruction bir:instruction))
  (loop for output in (bir:outputs inst)
        do (flow-datum domain output (supremum domain))))

(defmethod interpret-instruction ((domain backward-data)
                                  (instruction bir:instruction))
  (loop for input in (bir:inputs inst)
        do (flow-datum domain input (supremum domain))))

(defmethod interpret-instruction ((domain forward-data) (inst bir:jump))
  (loop for inp in (bir:inputs inst)
        for info = (info domain inp)
        for oup in (bir:outputs inst)
        do (flow-datum domain oup info)))

(defmethod interpret-instruction ((domain backward-data) (inst bir:jump))
  (loop for inp in (bir:inputs inst)
        for oup in (bir:outputs inst)
        for info = (info domain oup)
        do (flow-datum domain inp info)))

(defmethod interpret-instruction ((domain forward-data) (inst bir:unwind))
  (loop for inp in (bir:inputs inst)
        for info = (info domain inp)
        for oup in (bir:outputs inst)
        do (flow-datum domain oup info)))

(defmethod interpret-instruction ((domain backward-data) (inst bir:unwind))
  (loop for inp in (bir:inputs inst)
        for oup in (bir:outputs inst)
        for info = (info domain oup)
        do (flow-datum domain inp info)))

;;; functions:
;;; We associate domain information with each function. This represents the
;;; info for the return value for forward data, and for the arguments for
;;; backward data. The info is essentially averaged, in that for example for
;;; forward data, the eventual fixed point of the function output info will be
;;; the join of that obtainable by abstract interpretation from all call sites.
;;; This could be refined (FIXME).

(defmethod interpret-instruction ((domain forward-data) (inst bir:returni))
  (flow-datum domain (bir:function inst) (info domain (bir:input inst))))

(defmethod interpret-instruction ((domain backward-data) (inst bir:returni))
  (flow-datum domain (bir:input inst) (info domain (bir:function inst))))

;;; For local-call we need values, so that's in values-data, but for mv calls
;;; we can proceed simply.
(defmethod interpret-instruction ((domain forward-data)
                                  (inst bir:mv-local-call))
  (let ((callee (bir:callee inst)))
    (flow-call domain callee (info domain (second (bir:inputs inst))))
    (flow-datum domain (bir:output inst) (info domain callee))))

(defmethod flow-call ((domain backward-data) (function bir:function) info)
  (flow-datum domain function info)
  (let ((returni (bir:returni function)))
    (when returni (mark returni))))

(defmethod interpret-instruction ((domain backward-data)
                                  (inst bir:mv-local-call))
  (let ((callee (bir:callee inst)))
    (flow-call domain callee (info domain (bir:output inst)))
    (flow-datum domain (second (bir:inputs inst)) (info domain callee))))
