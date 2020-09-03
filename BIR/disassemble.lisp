(in-package #:cleavir-bir)

(defvar *disassemble-nextn*)
(defvar *disassemble-ids*)

(defun dis-id (value)
  (or (gethash value *disassemble-ids*)
      (setf (gethash value *disassemble-ids*)
            (prog1 *disassemble-nextn*
              (incf *disassemble-nextn*)))))

(defgeneric dis-label (instruction))
(defmethod dis-label ((instruction instruction))
  (class-name (class-of instruction)))

(defgeneric disassemble-variable (variable))
(defmethod disassemble-variable ((v variable)) (list (dis-id v)))

(defgeneric disassemble-value (value))

(defmethod disassemble-value ((value argument)) (dis-id value))
(defmethod disassemble-value ((value constant)) `',(constant-value value))
(defmethod disassemble-value ((value computation)) (dis-id value))

(defgeneric disassemble-instruction (instruction))

(defmethod disassemble-instruction ((inst operation))
  (list* (dis-label inst) (mapcar #'disassemble-value (inputs inst))))

(defmethod disassemble-instruction ((inst writevar))
  (list* (dis-label inst) (disassemble-variable (variable inst))
         (mapcar #'disassemble-value (inputs inst))))

(defmethod disassemble-instruction ((inst computation))
  `(:= ,(disassemble-value inst)
       ,(list* (dis-label inst) (mapcar #'disassemble-value (inputs inst)))))

(defmethod disassemble-instruction ((inst readvar))
  `(:= ,(disassemble-value inst)
       ,(list* (dis-label inst)
               (disassemble-variable (variable inst))
               (mapcar #'disassemble-value (inputs inst)))))

(defun disassemble-iblock (iblock)
  (check-type iblock iblock)
  (let ((insts nil))
    (mapnil-instructions (lambda (i) (push (disassemble-instruction i) insts))
                         (start iblock))
    (list* iblock (nreverse insts))))

(defun disassemble-function (function)
  (check-type function function)
  (let ((iblocks nil)
        (*disassemble-ids* (make-hash-table :test #'eq))
        (*disassemble-nextn* 0))
    (mapset (lambda (b) (push (disassemble-iblock b) iblocks))
            (iblocks function))
    (list* function iblocks)))
