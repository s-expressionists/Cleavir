(in-package #:cleavir-bir)

(defvar *ids*)
(defvar *name-ids*)

;;; Control parameters

(defvar cleavir-bir-disassembler:*show-dynenv* t
  "When true, the disassembler will display the dynamic environments of iblocks.")
(defvar cleavir-bir-disassembler:*show-ctype* nil
  "When true, the disassembler will display the inferred ctypes of data.")

;;; More advanced usage

(defvar *in-disassembly* nil)
(defmacro cleavir-bir-disassembler:with-disassembly ((&key override) &body body)
  "All disassemble operations in the dynamic extent of a WITH-DISASSEMBLY form will share names, etc. You can use this to display only particular regions of interest."
  (let ((bodyf (gensym "BODY")))
    `(flet ((,bodyf () ,@body))
       (if (or ,override (not *in-disassembly*))
           (let ((*in-disassembly* t)
                 (*name-ids* (make-hash-table :test #'equal))
                 (*ids* (make-hash-table :test #'eq)))
             (,bodyf))
           (,bodyf)))))

;; Prevents collisions by adding -## at the end.
(defun name-id (name)
  (multiple-value-bind (value presentp) (gethash name *name-ids*)
    (cond ((null name)
           (setf (gethash name *name-ids*) (if presentp (1+ value) 1))
           (make-symbol (write-to-string (if presentp value 0))))
          (presentp
           (setf (gethash name *name-ids*) (1+ value))
           (make-symbol (concatenate 'string
                                     (write-to-string name :escape nil)
                                     "-"
                                     (write-to-string value :escape nil))))
          (t
           (setf (gethash name *name-ids*) 0)
           (make-symbol (write-to-string name :escape nil))))))

(defgeneric cleavir-bir-disassembler:disassemble (bir)
  (:documentation "Return an s-expression representation of a BIR object."))

(defgeneric disassemble-datum (datum))
(defmethod disassemble-datum ((value constant)) `',(constant-value value))
(defmethod disassemble-datum ((value function-cell))
  `(function-cell ,(function-name value)))
(defmethod disassemble-datum ((value variable-cell))
  `(variable-cell ,(variable-name value)))
(defmethod disassemble-datum ((value datum))
  (or (gethash value *ids*)
      (setf (gethash value *ids*)
            (name-id (name value)))))
(defmethod disassemble-datum ((datum come-from))
  (name-id (or (name datum) 'unnamed-tag)))

(defun label (instruction)
  (class-name (class-of instruction)))

(defgeneric disassemble-instruction-extra (instruction)
  (:method-combination append)
  (:method append ((instruction instruction)) ()))

;;;; Instructions are disassembled as
;;;; (:= (...outputs...) (class-name ...inputs... ...extra...))
;;;; where: CLASS-NAME is the name of the instruction's class,
;;;; INPUTS is a list of data (made into symbols as described),
;;;; EXTRA is the list returned by DISASSEMBE-INSTRUCTION-EXTRA, a generic
;;;  function that can be specialized for different instruction-specific info,
;;;; OUTPUTS is a list of (symbol [ctype]), where SYMBOL is the datum's symbol,
;;;; and CTYPE is the datum's ctype, if the output is linear.

(defmethod cleavir-bir-disassembler:disassemble ((instruction instruction))
  (cleavir-bir-disassembler:with-disassembly ()
    (let ((dis `(,(label instruction)
                 ,@(mapcar #'disassemble-datum (inputs instruction))
                 ,@(disassemble-instruction-extra instruction))))
      `(:= (,@(mapcar (lambda (out)
                        `(,(disassemble-datum out)
                          ,@(when (typep out 'linear-datum)
                              `(,(ctype out)))))
                      (outputs instruction)))
           ,dis))))

(defun iblock-id (iblock)
  (or (gethash iblock *ids*)
      (setf (gethash iblock *ids*)
            (name-id (name iblock)))))

(defmethod disassemble-instruction-extra append ((inst primop))
  (list (primop-info:name (info inst))))

(defmethod disassemble-instruction-extra append ((inst terminator))
  (let ((n (mapcar #'iblock-id (next inst))))
    (if (null n)
        nil
        (list (mapcar #'iblock-id (next inst))))))

(defmethod disassemble-instruction-extra append ((inst enclose))
  (list (disassemble-datum (code inst))))

(defmethod disassemble-instruction-extra append ((inst come-from))
  (list (disassemble-datum inst)))

(defmethod disassemble-instruction-extra append ((inst unwind))
  (list (disassemble-datum (come-from inst))
        (iblock-id (destination inst))))

(defmethod disassemble-instruction-extra append ((inst load-time-value))
  (list (form inst) (read-only-p inst)))

(defmethod disassemble-instruction-extra append ((inst typeq-test))
  (list (test-ctype inst)))

(defmethod disassemble-instruction-extra append ((inst thei))
  (list (asserted-type inst) (type-check-function inst)))

(defun disassemble-dynenv (dynenv)
  (etypecase dynenv
    (function `(:function ,(disassemble-datum dynenv)))
    (instruction `(,(label dynenv) :in ,(name (iblock dynenv))))))

(defmethod cleavir-bir-disassembler:disassemble ((iblock iblock))
  (check-type iblock iblock)
  (cleavir-bir-disassembler:with-disassembly ()
    (let ((insts nil))
      (do-iblock-instructions (i iblock)
        (push (cleavir-bir-disassembler:disassemble i) insts))
      (list* (list* (iblock-id iblock)
                    (mapcar #'disassemble-datum (inputs iblock)))
             (disassemble-dynenv (dynamic-environment iblock))
             (set:mapset 'list #'iblock-id (entrances iblock))
             (nreverse insts)))))

(defun disassemble-lambda-list (ll)
  (loop for item in ll
        collect (cond ((member item lambda-list-keywords) item)
                      ((typep item 'argument)
                       (disassemble-datum item))
                      ((= (length item) 3)
                       (list (first item)
                             (disassemble-datum (second item))
                             (disassemble-datum (third item))))
                      (t (mapcar #'disassemble-datum item)))))

(defmethod cleavir-bir-disassembler:disassemble ((function function))
  (check-type function function)
  (cleavir-bir-disassembler:with-disassembly ()
    (let ((iblocks nil))
      ;; sort blocks in forward flow order.
      (do-iblocks (iblock function :backward)
        (push (cleavir-bir-disassembler:disassemble iblock) iblocks))
      (list* (list (disassemble-datum function) (iblock-id (start function))
                   (disassemble-lambda-list (lambda-list function))
                   (set:mapset 'list #'disassemble-datum
                               (environment function)))
             iblocks))))

(defmethod cleavir-bir-disassembler:disassemble ((module module))
  (check-type module module)
  (cleavir-bir-disassembler:with-disassembly ()
    (list* (set:mapset 'list #'disassemble-datum (constants module))
           (set:mapset 'list #'cleavir-bir-disassembler:disassemble
                       (functions module)))))

(defun cleavir-bir-disassembler:display-instruction-disassembly
    (inst-disasm &key (show-ctype cleavir-bir-disassembler:*show-ctype*))
  "Given the s-expression representation of a BIR instruction (obtained via DISASSEMBLE), print a textual representation to standard output."
  (destructuring-bind (assign outs . rest) inst-disasm
    (declare (cl:ignore assign))
    (format t "~&     ")
    (format t "~{~(~a~)~}" rest)
    (when outs
      (format t " -> "))
    (format t "~{~a~^, ~}" (mapcar #'first outs))
    (when show-ctype
      (let* ((type-specs (mapcar #'cdr outs))
             (types (mapcar #'first type-specs)))
        (unless (or (every (lambda (ctype) (cleavir-ctype:top-p ctype nil))
                           types)
                    (every (lambda (type-spec) (eq type-spec nil)) type-specs))
          (format t "~45T; ")
          (format t "~{ctype: ~a~^, ~}" types)))))
  (values))

(defun cleavir-bir-disassembler:display-iblock-disassembly
    (iblock-disasm &key
                     (show-dynenv cleavir-bir-disassembler:*show-dynenv*)
                     ((:show-ctype cleavir-bir-disassembler:*show-ctype*)
                      cleavir-bir-disassembler:*show-ctype*))
  "Given the s-expression representation of a BIR iblock (obtained via DISASSEMBLE), print a textual representation to standard output."
  (destructuring-bind ((label . args) dynenv entrances &rest insts)
      iblock-disasm
    (format t "~&  iblock ~a ~:a:" label args)
    (when show-dynenv
      (format t "~&   dynenv = ~a" dynenv))
    (when entrances
      (format t "~&   entrances = ~(~:a~)" entrances))
    (mapc #'cleavir-bir-disassembler:display-instruction-disassembly insts))
  (values))

(defun cleavir-bir-disassembler:display-function-disassembly
    (function-disasm &key
                     ((:show-dynenv cleavir-bir-disassembler:*show-dynenv*)
                      cleavir-bir-disassembler:*show-dynenv*)
                     ((:show-ctype cleavir-bir-disassembler:*show-ctype*)
                      cleavir-bir-disassembler:*show-ctype*))
  "Given the s-expression representation of a BIR function (obtained via DISASSEMBLE), print a textual representation to standard output."
  (destructuring-bind ((name start args env) . iblocks)
      function-disasm
    (format t "~&function ~a ~:a ~&     with environment ~(~:a~) ~&     with start iblock ~a"
            name args env start)
    (mapc #'cleavir-bir-disassembler:display-iblock-disassembly iblocks))
  (values))

(defun cleavir-bir-disassembler:display-module-disassembly
    (disasm &key
              ((:show-dynenv cleavir-bir-disassembler:*show-dynenv*)
               cleavir-bir-disassembler:*show-dynenv*)
              ((:show-ctype cleavir-bir-disassembler:*show-ctype*)
               cleavir-bir-disassembler:*show-ctype*))
  "Given the s-expression representation of a BIR module (obtained via DISASSEMBLE), print a textual representation to standard output."
  (format t "~&-------module-------")
  (destructuring-bind (constants . funs) disasm
    (format t "~&constants: ~:s" constants)
    (mapc #'cleavir-bir-disassembler:display-function-disassembly funs))
  (values))

(defgeneric cleavir-bir-disassembler:display (bir &key)
  (:documentation "Print a textual representation of the BIR object to standard output.
This is the main entry point to the disassembler."))
(defmethod cleavir-bir-disassembler:display
    ((module module)
     &key ((:show-dynenv cleavir-bir-disassembler:*show-dynenv*)
           cleavir-bir-disassembler:*show-dynenv*)
       ((:show-ctype cleavir-bir-disassembler:*show-ctype*)
        cleavir-bir-disassembler:*show-ctype*))
  (cleavir-bir-disassembler:display-module-disassembly
   (cleavir-bir-disassembler:disassemble module)))
(defmethod cleavir-bir-disassembler:display
    ((function function)
     &key ((:show-dynenv cleavir-bir-disassembler:*show-dynenv*)
           cleavir-bir-disassembler:*show-dynenv*)
       ((:show-ctype cleavir-bir-disassembler:*show-ctype*)
        cleavir-bir-disassembler:*show-ctype*))
  (cleavir-bir-disassembler:display-function-disassembly
   (cleavir-bir-disassembler:disassemble function)))
(defmethod cleavir-bir-disassembler:display
    ((iblock iblock)
     &key ((:show-dynenv cleavir-bir-disassembler:*show-dynenv*)
           cleavir-bir-disassembler:*show-dynenv*)
       ((:show-ctype cleavir-bir-disassembler:*show-ctype*)
        cleavir-bir-disassembler:*show-ctype*))
  (cleavir-bir-disassembler:display-iblock-disassembly
   (cleavir-bir-disassembler:disassemble iblock)))
(defmethod cleavir-bir-disassembler:display
    ((instruction instruction)
     &key ((:show-ctype cleavir-bir-disassembler:*show-ctype*)
           cleavir-bir-disassembler:*show-ctype*))
  (cleavir-bir-disassembler:display-instruction-disassembly
   (cleavir-bir-disassembler:disassemble instruction)))
