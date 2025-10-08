(in-package #:maclina->bir)

;;;; Convert a Maclina compiled module to BIR.
;;;; This is easier in some ways, since we haven't erased some information,
;;;; e.g. what constants represent.

;;; Process a bytecode cmp:module's literal infos into something easy to turn into
;;; BIR. BIR is computed eagerly, unlike compute-runtime-literals, because it makes
;;; translation easier. And optimization can still remove constants.
(defgeneric compute-compiled-literal (info module))
(defmethod compute-compiled-literal ((info maclina.compile:constant-info) module)
  (cons info (bir:constant-in-module (maclina.compile:constant-info-value info)
                                     module)))
(defmethod compute-compiled-literal ((info maclina.compile:cfunction) module)
  (declare (ignore module))
  (cons info :cfunction))
(defmethod compute-compiled-literal ((info maclina.compile:ltv-info) module)
  (cons info (bir:load-time-value-in-module
              (maclina.compile:ltv-info-form info)
              (maclina.compile:ltv-info-read-only-p info)
              module)))
(defmethod compute-compiled-literal ((info maclina.compile:fdefinition-info) module)
  (cons info (bir:function-cell-in-module (maclina.compile:fdefinition-info-name info)
                                          module)))
(defmethod compute-compiled-literal ((info maclina.compile:value-cell-info) module)
  (cons info (bir:variable-cell-in-module (maclina.compile:value-cell-info-name info)
                                          module)))
(defmethod compute-compiled-literal ((info maclina.compile:env-info) module)
  ;; FIXME? There's a bit of a mismatch here. Native-compiled code probably
  ;; never refers to the environment, for now.
  (cons info (bir:constant-in-module nil module)))

(defun compute-compiled-literals (literals irmodule)
  (map 'vector (lambda (lit) (compute-compiled-literal lit irmodule)) literals))

(defmethod start-annotation ((annot maclina.compile:cfunction) context)
  (begin-function annot context))
(defmethod end-annotation ((annot maclina.compile:cfunction) context)
  (end-function annot context))

(defmethod locals-size ((fun maclina.compile:cfunction))
  (maclina.compile:cfunction-nlocals fun))
(defmethod nvars ((fun maclina.compile:cfunction))
  (length (maclina.compile:cfunction-closed fun)))
(defmethod fname ((fun maclina.compile:cfunction))
  (maclina.compile:cfunction-name fun))
(defmethod lambda-list ((fun maclina.compile:cfunction))
  (maclina.compile:cfunction-lambda-list fun))

(defmethod fcell/name ((cell maclina.compile:fdefinition-info))
  (maclina.compile:fdefinition-info-name cell))
(defmethod vcell/name ((cell maclina.compile:value-cell-info))
  (maclina.compile:value-cell-info-name cell))

(defmethod constant-value ((const maclina.compile:constant-info))
  (maclina.compile:constant-info-value const))
;; The actual values in the literals will be (cmp . ir) pairs.
(defmethod constant-value ((const cons))
  (constant-value (car const)))

(defun compile-cmodule-into (client bytecode literals-info pc-map irmodule)
  (let ((literals (compute-compiled-literals literals-info irmodule)))
    (values irmodule
            (fmap (compile-bytecode-into client bytecode pc-map literals irmodule))
            literals)))
