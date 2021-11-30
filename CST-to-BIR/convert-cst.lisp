(in-package #:cleavir-cst-to-bir)

(defmethod convert-cst
    (cst (info env:symbol-macro-info) inserter env system)
  (let* ((expansion (env:expansion info))
         (expander (symbol-macro-expander expansion))
         (expanded-form (expand-macro expander cst env))
         (expanded-cst (cst:reconstruct expanded-form cst system)))
    (convert expanded-cst inserter env system)))

(defmethod convert-cst
    (cst (info env:constant-variable-info) inserter env system)
  (let ((cst (cst:cst-from-expression (env:value info)
                                      :source (cst:source cst))))
    (convert-constant cst inserter env system)))

(defmethod convert-cst
    (cst (info env:special-operator-info) inserter env system)
  (convert-special (car (cst:raw cst)) cst inserter env system))

(defmethod convert-cst
    (cst (info env:local-macro-info) inserter env system)
  (let* ((expander (env:expander info))
         (expanded-form (expand-macro expander cst env))
         (expanded-cst (cst:reconstruct expanded-form cst system)))
    (convert expanded-cst inserter env system)))

(defmethod convert-cst
    (cst (info env:global-macro-info) inserter env system)
  (let ((compiler-macro (env:compiler-macro info))
        (notinline (eq 'notinline (env:inline info)))
        (expander (env:expander info)))
    (if (or notinline (null compiler-macro))
        (let* ((expanded-form (expand-macro expander cst env))
               (expanded-cst (cst:reconstruct expanded-form cst system)))
          (convert expanded-cst inserter env system))
        (let ((expanded-form (expand-compiler-macro compiler-macro cst env)))
          (if (eq (cst:raw cst) expanded-form)
              (let* ((expanded-form
                       (expand-macro expander cst env))
                     (expanded-cst (cst:reconstruct expanded-form cst system)))
                (convert expanded-cst inserter env system))
              (let ((expanded-cst (cst:reconstruct expanded-form cst system)))
                (convert expanded-cst inserter env system)))))))

;;; TODO: Type declarations
(defun make-call (cst info arguments-cst inserter env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (let* ((name-cst (cst:first cst))
         (callee
          (convert-called-function-reference name-cst info inserter env system)))
    (with-compiled-arguments (args arguments-cst inserter env system)
      (let ((call-out (make-instance 'bir:output)))
        (insert inserter (make-instance 'bir:call
                           :inputs (list* (first callee) args)
                           :outputs (list call-out)))
        (list call-out)))))

(defmethod convert-cst
    (cst (info env:global-function-info) inserter env system)
  (let ((compiler-macro (env:compiler-macro info))
        (notinline (eq 'notinline (env:inline info))))
    (if (or notinline (null compiler-macro))
        (make-call cst info (cst:rest cst) inserter env system)
        (let ((expanded-form (expand-compiler-macro compiler-macro cst env)))
          (if (eq (cst:raw cst) expanded-form)
              (make-call cst info (cst:rest cst) inserter env system)
              (let ((expanded-cst (cst:reconstruct expanded-form cst system)))
                (convert expanded-cst inserter env system)))))))

(defmethod convert-cst
    (cst (info env:local-function-info) inserter env system)
  (make-call cst info (cst:rest cst) inserter env system))

(defmethod convert-special-variable (cst info inserter global-env system)
  (declare (ignore global-env system))
  (let* ((symbol (env:name info))
         (origin (cst:source cst))
         (cr (convert-constant (cst:cst-from-expression
                                symbol :source origin)
                               inserter))
         (outs (list (make-instance 'bir:output :name symbol))))
    (insert inserter
            (make-instance 'cleavir-bir:primop
              :info (cleavir-primop-info:info 'symbol-value)
              :inputs cr :outputs outs))
    (copy-list outs)))

(defmethod convert-cst
    (cst (info env:special-variable-info) inserter env system)
  (let ((global-env (env:global-environment env)))
    (convert-special-variable cst info inserter global-env system)))

(defmethod convert-cst
    (cst (info env:lexical-variable-info) inserter env system)
  (declare (ignore env system))
  (when (eq (env:ignore info) 'ignore)
    (warn 'ignored-variable-referenced :cst cst))
  (let ((var (env:identity info)))
    (typecase var
      (bir:argument (list var))
      (t
       (bir:record-variable-ref var)
       (let ((rv-out (make-instance 'bir:output :name (env:name info))))
         (insert inserter (make-instance 'bir:readvar
                            :inputs (list var) :outputs (list rv-out)))
         (list rv-out))))))
