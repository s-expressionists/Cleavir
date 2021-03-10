(cl:in-package #:cleavir.bir.visualizer)

;;; Datum utilities

(defvar *datum-numbers*)

(defun datum-number (datum)
  (let ((numbers *datum-numbers*))
    (a:ensure-gethash datum numbers (1+ (hash-table-count numbers)))))

(defun datum-ink (datum)
  (let ((number (typecase datum
                  (integer datum)
                  (t       (datum-number datum)))))
    (clim:make-contrasting-inks 8 (mod number 8))))

(defclass datum-place (clouseau:pseudo-place)
  ())

(defun inspect-datum (datum container stream)
  (clouseau:formatting-place
      (container 'datum-place datum nil present-object)
    (present-object stream)))

;;; Type utilities

(deftype single-value-type-specifier ()
  '(cons (eql values) (cons t (cons (eql &optional) null))))

(defun print-ctype (ctype stream)
  (let* ((specifier (sb-c::type-specifier ctype))
         (specifier (if (typep specifier 'single-value-type-specifier)
                        (second specifier)
                        specifier)))
    (let ((*print-right-margin* most-positive-fixnum))
      (princ specifier stream))))

(defun print-type-annotation (ctype stream)
  (clim:with-drawing-options (stream :text-size :tiny :ink clim:+dark-green+)
    (write-char #\Space stream)
    (princ ctype stream)))

;;; `module'

(defmethod clouseau:inspect-object-using-state ((object bir:module)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-body))
                                                (stream clim:extended-output-stream))
  (let ((*datum-numbers* (make-hash-table :test #'eq)))
    (clim:formatting-table (stream :x-spacing 16)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream)
          (clouseau:with-section (stream) "Constants"
            (let ((constants (bir:constants object)))
              (clouseau:with-placeholder-if-empty (stream)
                ((set:empty-set-p constants)
                 "No Constants")
                (t
                 (clim:formatting-item-list (stream :n-columns 1)
                   (cleavir-set:doset (constant constants)
                     (clim:formatting-cell (stream)
                       (clouseau:formatting-place
                           (object 'clouseau:pseudo-place constant nil present-object)
                         (present-object stream))))))))))
        (clim:formatting-cell (stream)
          (clouseau:with-section (stream) "Functions"
            (clim:formatting-item-list (stream)
              (bir:do-functions (function object)
                (clim:formatting-cell (stream)
                  (clouseau:formatting-place (object 'clouseau:pseudo-place function nil present-object)
                    (present-object stream)))))))))
    (terpri stream)
    ;; Instance slots
    (call-next-method)))

;;; `function'
;;;
;;; Presented as name and lambda list in the header and the BIR
;;; control flow graph in the body. Slots of the instance are
;;; initially hidden.

(defmethod clouseau:make-object-state ((object bir:function) (place t))
  ;; When the function is inspected as the content of a slot, use
  ;; defaults. Otherwise pre-expand and hide slots.
  (if (typep place 'clouseau:pseudo-place)
      (let ((class (clouseau:object-state-class object place)))
        (make-instance class :place      place
                             :style      :expanded
                             :slot-style nil))
      (call-next-method)))

(defmethod clouseau:inspect-object-using-state ((object bir:function)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-header))
                                                (stream clim:extended-output-stream))
  (call-next-method)
  (let ((*print-right-margin* most-positive-fixnum))
    (format stream " ~A ~S" (bir:name object) (bir:lambda-list object))))

(defmethod clouseau:inspect-object-using-state ((object bir:function)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-body))
                                                (stream clim:extended-output-stream))
  (clouseau:with-section (stream) "BIR Control Flow Graph"
    (clim:format-graph-from-root
     (bir:start object)
     (lambda (node stream)
       (clouseau:formatting-place
           (object 'clouseau:pseudo-place node nil present-object)
         (present-object stream)))
     (lambda (node)
       (a:ensure-list (bir:next (bir:end node))))
     :merge-duplicates t :duplicate-test #'eq
     :maximize-generations t
     :stream stream))
  ;; Instance slots
  (call-next-method))

;;; `iblock'
;;;
;;; Presented as a vertical list of instructions. Presented
;;; pre-expanded with hidden slots when the context is the control
;;; flow graph of a function.

(defmethod clouseau:make-object-state ((object bir:iblock) (place t))
  ;; When the block is inspected as the content of a slot, use
  ;; defaults. Otherwise pre-expand and hide slots.
  (if (typep place 'clouseau:pseudo-place)
      (let ((class (clouseau:object-state-class object place)))
        (make-instance class :place      place
                             :style      :expanded
                             :slot-style nil))
      (call-next-method)))

(defmethod clouseau:inspect-object-using-state ((object bir:iblock)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (format stream "#<~A ~A>"
          (symbol-name (class-name (class-of object)))
          (bir:name object)))

(defmethod clouseau:inspect-object-using-state ((object bir:iblock)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-header))
                                                (stream clim:extended-output-stream))
  (clouseau::inspect-class-as-name (class-of object) stream)
  (format stream " ~A " (bir:name object)))

(defmethod clouseau:inspect-object-using-state ((object bir:iblock)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-body))
                                                (stream clim:extended-output-stream))
  (clouseau:with-section (stream) "Instructions"
    (bir:do-iblock-instructions (instruction object)
      (fresh-line stream)
      (clouseau:formatting-place
          (object 'clouseau:pseudo-place instruction nil present-object)
        (present-object stream))))
  (terpri stream)
  ;; Instance slots
  (call-next-method))

;;; `instruction'

(defclass inspected-instruction (clouseau:inspected-instance)
  ())

(defmethod clouseau:object-state-class ((object bir:instruction)
                                        (place  datum-place))
  'clouseau:inspected-instance)

(defmethod clouseau:object-state-class ((object bir:instruction)
                                        (place  t))
  'inspected-instruction)

(defmethod clouseau:inspect-object-using-state ((object bir:instruction)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (clim:with-drawing-options (stream :text-family :fix :ink clim:+dark-blue+)
    (format stream "~(~A~)" (symbol-name (class-name (class-of object))))))

(defmethod clouseau:inspect-object-using-state ((object bir:instruction)
                                                (state  inspected-instruction)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (call-next-method)
  ;; Inputs
  (loop :for input :in (bir:inputs object)
        :do (write-char #\Space stream)
            (inspect-datum input object stream))
  ;; Outputs
  (a:when-let ((outputs (remove object (bir:outputs object))))
    (write-string " →" stream)
    (loop :for output :in outputs
          :do (write-char #\Space stream)
              (inspect-datum output object stream))))

(defmethod clouseau:inspect-object-using-state :around ((object bir:linear-datum)
                                                        (state  inspected-instruction)
                                                        (style  (eql :collapsed))
                                                        (stream clim:extended-output-stream))
  (call-next-method)
  (print-type-annotation (bir:ctype object) stream))

(defmethod clouseau:inspect-object-using-state ((object bir:primop)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (clim:with-drawing-options (stream :text-family :fix :ink clim:+dark-red+)
    (format stream "~(~A~)" (cleavir-primop-info:name (bir:info object)))))

;;; `datum'
;;;
;;; We just make up a number and corresponding color. The number is
;;; used for datums that do not have a name or some other identifying
;;; feature.

(defmethod clouseau:inspect-object-using-state ((object bir:datum)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (if (typep object '(or bir:function bir:instruction)) ; HACK
      (call-next-method)
      (let ((number (datum-number object)))
        (clim:with-drawing-options (stream :ink (datum-ink number))
          (format stream "~D" (or (bir:name object) number))))))

(defmethod clouseau:inspect-object-using-state ((object bir:constant)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (let ((number (datum-number object)))
    (clim:with-drawing-options (stream :ink (datum-ink number))
      (format stream "~S" (bir:constant-value object)))))
