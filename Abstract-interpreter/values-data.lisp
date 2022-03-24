(in-package #:cleavir-abstract-interpreter)

;;;; Forward and backward data flows for values data.

(defmethod interpret-instruction ((strategy strategy)
                                  (domain forward-values-data) (product product)
                                  (inst bir:fixed-to-multiple))
  (loop for inp in (bir:inputs inst)
        collect (primary domain (info strategy domain inp)) into in-infos
        finally (flow-datum strategy domain
                            (bir:output inst)
                            (ftm-info domain in-infos))))

(defmethod interpret-instruction ((strategy strategy)
                                  (domain backward-values-data) (product product)
                                  (inst bir:fixed-to-multiple))
  (loop with output = (bir:output inst)
        with out-info = (info strategy domain output)
        for inp in (bir:inputs inst)
        for i from 0
        for sinfo = (info-values-nth domain i out-info)
        for vinfo = (single-value domain sinfo)
        do (flow-datum strategy domain inp vinfo)))

(defmethod interpret-instruction ((strategy strategy)
                                  (domain forward-values-data) (product product)
                                  (inst bir:writevar))
  (let* ((input (bir:input inst)) (variable (bir:output inst))
         (input-info (primary domain (info strategy domain input)))
         (vinput-info (single-value domain input-info)))
    (flow-datum strategy domain variable vinput-info)))

(defmethod interpret-instruction ((strategy strategy)
                                  (domain backward-values-data) (product product)
                                  (inst bir:writevar))
  (flow-datum strategy domain (bir:input inst)
              (info strategy domain (bir:output inst))))

(defmethod interpret-instruction ((strategy strategy)
                                  (domain forward-values-data) (product product)
                                  (inst bir:readvar))
  (flow-datum strategy domain (bir:output inst)
              (info strategy domain (bir:input inst))))

(defmethod interpret-instruction ((strategy strategy)
                                  (domain backward-values-data) (product product)
                                  (inst bir:readvar))
  (let* ((variable (bir:input inst)) (output (bir:output inst))
         (output-info (primary domain (info strategy domain output)))
         (voutput-info (single-value domain output-info)))
    (flow-datum strategy domain variable voutput-info)))

;;; functions. see data.lisp for more information.

(defmethod flow-call ((strategy strategy) (domain forward-values-data)
                      (function bir:function) info)
  (let ((svtop (single-value domain (sv-supremum domain))))
    (bir:map-lambda-list
     (lambda (state item index)
       (ecase state
         ((:required)
          (flow-datum strategy domain item
                      (single-value domain
                                    (info-values-nth domain index info))))
         ((&optional)
          (flow-datum strategy domain (first item)
                      (single-value domain
                                    (info-values-nth domain index info)))
          ;; Dunno what's going on with -p, so default.
          (flow-datum strategy domain (second item) svtop))
         ((&rest) (flow-datum strategy domain item svtop))
         ((&key)
          (flow-datum strategy domain (second item) svtop)
          (flow-datum strategy domain (third item) svtop))))
     (bir:lambda-list function))))

(defmethod interpret-instruction ((strategy strategy)
                                  (domain forward-values-data) (product product)
                                  (inst bir:local-call))
  (let ((callee (bir:callee inst)) (args (rest (bir:inputs inst))))
    (flow-call strategy domain callee
               (ftm-info domain
                         (loop for arg in args
                               for info = (info strategy domain arg)
                               collect (primary domain info))))
    (flow-datum strategy domain (bir:output inst)
                (info strategy domain callee))))

(defun flow-arguments (strategy domain lambda-list args)
  (flet ((svp (arg) (single-value domain
                                  (primary domain (info strategy domain arg)))))
    (bir:map-lambda-list
     (lambda (state item index)
       (declare (ignore index))
       (case state
         ((:required) (flow-datum strategy domain (pop args) (svp item)))
         ((&optional)
          (when args (flow-datum strategy domain (pop args)
                                 (svp (first item)))))))
     lambda-list)
    ;; Any remaining parameters are &rest or &key or something, and we have no
    ;; coherent information about those, so default. FIXME: Could be refined.
    (loop with svtop = (single-value domain (sv-supremum domain))
          for arg in args
          do (flow-datum strategy domain arg svtop))))

(defmethod interpret-instruction ((strategy strategy)
                                  (domain backward-values-data) (product product)
                                  (inst bir:local-call))
  (let* ((callee (bir:callee inst)) (args (rest (bir:inputs inst))))
    (flow-arguments strategy domain (bir:lambda-list callee) args)
    (flow-call strategy domain callee
               (info strategy domain (bir:output inst)))))
