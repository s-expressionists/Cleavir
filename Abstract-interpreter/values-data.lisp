(in-package #:cleavir-abstract-interpreter)

;;;; Forward and backward data flows for values data.

;;; Mark arguments as sv-sup.
(defmethod initialize-entry-point ((strategy strategy) (domain forward-values-data)
                                   (function bir:function))
  ;; Make the arguments supreme.
  (let ((sup (single-value domain (sv-supremum domain))))
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

(defmethod flow-instruction ((domain forward-values-data) (inst bir:fixed-to-multiple)
                             &rest in-infos)
  (ftm-info domain (mapcar (lambda (i) (primary domain i)) in-infos)))

(defmethod flow-instruction ((domain backward-values-data) (inst bir:fixed-to-multiple)
                             &rest in-infos)
  (values-list (loop with info = (first in-infos)
                     for i from 0 below (length (bir:inputs inst))
                     collect (single-value domain (info-values-nth domain i info)))))

(defmethod flow-instruction ((domain forward-values-data) (inst bir:writevar)
                             &rest in-infos)
  (single-value domain (primary domain (first in-infos))))
(defmethod flow-instruction ((domain backward-values-data) (inst bir:readvar)
                             &rest in-infos)
  (single-value domain (primary domain (first in-infos))))
