(in-package #:cleavir-cst-to-bir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.

(defmethod convert-setq
    (var-cst form-cst (info env:constant-variable-info)
     inserter env system)
  (declare (ignore var-cst inserter env system))
  (error 'setq-constant-variable :cst form-cst))

(defmethod convert-setq
    (var-cst form-cst (info env:lexical-variable-info)
     inserter env system)
  (declare (ignore var-cst))
  (let ((var (env:identity info)))
    (bir:record-variable-set var)
    (with-compiled-cst (rv form-cst inserter env system)
      (insert inserter
              (make-instance 'bir:writevar
                :inputs rv :outputs (list var)))
      (let ((readvar-out (make-instance 'bir:output)))
        (insert inserter (make-instance 'bir:readvar
                           :inputs (list var) :outputs (list readvar-out)))
        (list readvar-out)))))

(defmethod convert-setq
    (var-cst form-cst (info env:symbol-macro-info) inserter env system)
  (let* ((expansion (env:expansion info))
         (expander (symbol-macro-expander expansion))
         (expanded-variable (expand-macro expander var-cst env))
         (expanded-cst (cst:reconstruct expanded-variable var-cst system))
         (origin (cst:source var-cst)))
    (convert (cst:quasiquote origin
                             (setf (cst:unquote expanded-cst)
                                   (cst:unquote form-cst)))
             inserter env system)))

(defmethod convert-setq
    (var-cst form-cst (info env:special-variable-info)
     inserter env system)
  ;; Basically, we rewrite (setq *foo* form) to
  ;; (let ((temp form)) (set '*foo* temp) temp)
  (with-compiled-cst (rv form-cst inserter env system)
    (let* ((name (cst:raw var-cst))
           (temp (make-instance 'bir:variable :name name))
           (tbind (make-instance 'bir:leti
                    :inputs rv :outputs (list temp)))
           (cr (convert-constant var-cst inserter env system))
           (read-out1 (make-instance 'bir:output :name name))
           (read1 (make-instance 'bir:readvar
                    :inputs (list temp) :outputs (list read-out1)))
           (set (make-instance 'cleavir-bir:primop
                  :info (cleavir-primop-info:info '(setf symbol-value))
                  :inputs cr :outputs ()))
           (read-out2 (make-instance 'bir:output :name name))
           (read2 (make-instance 'bir:readvar
                    :inputs (list temp) :outputs (list read-out2))))
      (adjoin-variable inserter temp)
      (insert inserter tbind)
      (setf (bir:binder temp) tbind)
      (insert inserter read1)
      (insert inserter set)
      (insert inserter read2)
      (list read-out2))))

(defun convert-elementary-setq (var-cst form-cst inserter env system)
  (convert-setq var-cst form-cst (variable-info system env var-cst)
                inserter env system))
