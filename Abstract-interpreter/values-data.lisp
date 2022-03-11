(in-package #:cleavir-abstract-interpreter)

;;;; Forward and backward data flows for values data.

(defmethod interpret-instruction ((domain forward-values-data)
                                  (inst bir:fixed-to-multiple))
  (loop for inp in (bir:inputs inst)
        collect (primary domain (info domain inp)) into in-infos
        finally (flow-datum domain
                            (bir:output inst)
                            (ftm-info domain in-infos))))

(defmethod interpret-instruction ((domain backward-values-data)
                                  (inst bir:fixed-to-multiple))
  (loop with output = (bir:output inst)
        with out-info = (info domain output)
        for inp in (bir:inputs inst)
        for i from 0
        for sinfo = (info-values-nth domain i out-info)
        for vinfo = (single-value domain sinfo)
        do (flow-datum domain inp vinfo)))

(defmethod interpret-instruction ((domain forward-values-data)
                                  (inst bir:writevar))
  (let* ((input (bir:input inst)) (variable (bir:output inst))
         (input-info (primary domain (info domain input)))
         (vinput-info (single-value domain input-info)))
    (flow-datum domain variable vinput-info)))

(defmethod interpret-instruction ((domain backward-values-data)
                                  (inst bir:writevar))
  (flow-datum domain (bir:input inst) (info domain (bir:output inst))))

(defmethod interpret-instruction ((domain forward-values-data)
                                  (inst bir:readvar))
  (flow-datum domain (bir:output inst) (info domain (bir:input inst))))

(defmethod interpret-instruction ((domain backward-values-data)
                                  (inst bir:readvar))
  (let* ((variable (bir:input inst)) (output (bir:output inst))
         (output-info (primary domain (info domain output)))
         (voutput-info (single-value domain output-info)))
    (flow-datum domain variable voutput-info)))

;;; functions. see data.lisp for more information.

(defmethod flow-call ((domain forward-values-data) (function bir:function) info)
  (let ((svtop (single-value domain (sv-supremum domain))))
    (bir:map-lambda-list
     (lambda (state item index)
       (ecase state
         ((:required)
          (flow-datum domain item (info-values-nth domain index info)))
         ((&optional)
          (flow-datum domain (first item) (info-values-nth domain index info))
          ;; Dunno what's going on with -p, so default.
          (flow-datum domain (second item) svtop))
         ((&rest) (flow-datum domain item svtop))
         ((&key)
          (flow-datum domain (second item) svtop)
          (flow-datum domain (third item) svtop))))
     (bir:lambda-list function))))

(defmethod interpret-instruction ((domain forward-values-data)
                                  (inst bir:local-call))
  (let ((callee (bir:callee inst)) (args (rest (bir:inputs inst))))
    (flow-call domain callee
               (ftm-info domain
                         (loop for arg in args
                               for info = (info domain arg)
                               collect (primary domain info))))
    (flow-datum domain (bir:output inst) (info domain callee))))

(defun flow-arguments (domain lambda-list args)
  (flet ((svp (arg) (single-value domain (primary domain (info domain arg)))))
    (bir:map-lambda-list
     (lambda (state item index)
       (declare (ignore index))
       (case state
         ((:required) (flow-datum domain (pop args) (svp item)))
         ((&optional)
          (when args (flow-datum domain (pop args) (svp (first item)))))))
     lambda-list)
    ;; Any remaining parameters are &rest or &key or something, and we have no
    ;; coherent information about those, so default. FIXME: Could be refined.
    (loop with svtop = (single-value domain (sv-supremum domain))
          for arg in args
          do (flow-datum domain arg svtop))))

(defmethod interpret-instruction ((domain backward-values-data)
                                  (inst bir:local-call))
  (let* ((callee (bir:callee inst)) (args (rest (bir:inputs inst))))
    (flow-arguments domain (bir:lambda-list callee) args)
    (flow-call domain callee (info domain (bir:output inst)))))
