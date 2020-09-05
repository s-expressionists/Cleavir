(in-package #:cleavir-bir)

(defvar *seen*)
(defvar *work*)

(defun maybe-add-disassemble-work (function)
  (check-type function function)
  (unless (presentp function *seen*)
    (push function *work*)))

(defvar *disassemble-nextv*)
(defvar *disassemble-vars*)

(defvar *disassemble-nextn*)
(defvar *disassemble-ids*)

(defun dis-id (value)
  (or (gethash value *disassemble-ids*)
      (setf (gethash value *disassemble-ids*)
            (prog1 *disassemble-nextn*
              (incf *disassemble-nextn*)))))

(defun dis-var (variable)
  (or (gethash variable *disassemble-vars*)
      (setf (gethash variable *disassemble-vars*)
            (prog1 (make-symbol (write-to-string *disassemble-nextv*))
              (incf *disassemble-nextv*)))))

(defgeneric dis-label (instruction))
(defmethod dis-label ((instruction instruction))
  (class-name (class-of instruction)))

(defgeneric disassemble-variable (variable))
(defmethod disassemble-variable ((v variable)) (dis-var v))

(defgeneric disassemble-value (value))

(defmethod disassemble-value ((value argument)) (dis-id value))
(defmethod disassemble-value ((value constant)) `',(constant-value value))
(defmethod disassemble-value ((value computation)) (dis-id value))
(defmethod disassemble-value ((value load-time-value))
  (if (and (read-only-p value) (constantp (form value)))
      `',(eval (form value))
      `(cl:load-time-value ,(form value) ,(read-only-p value))))

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

(defmethod disassemble-instruction ((inst enclose))
  (maybe-add-disassemble-work (code inst))
  `(:= ,(disassemble-value inst)
       ,(list* (dis-label inst)
               (code inst)
               (mapcar #'disassemble-value (inputs inst)))))

(defun disassemble-iblock (iblock)
  (check-type iblock iblock)
  (let ((insts nil))
    (map-iblock-instructions
     (lambda (i) (push (disassemble-instruction i) insts))
     (start iblock))
    (list* (list* iblock (mapcar #'disassemble-value (inputs iblock)))
           (nreverse insts))))

(defun disassemble-lambda-list (ll)
  (loop for item in ll
        collect (cond ((member item lambda-list-keywords) item)
                      ((typep item 'argument)
                       (disassemble-value item))
                      ((= (length item) 3)
                       (list (first item)
                             (disassemble-value (second item))
                             (disassemble-value (third item))))
                      (t (mapcar #'disassemble-value item)))))

(defun disassemble-function (function)
  (check-type function function)
  (refresh-iblocks function)
  (let ((iblocks nil)
        (*disassemble-ids* (make-hash-table :test #'eq))
        (*disassemble-nextn* 0))
    (mapset (lambda (b) (push (disassemble-iblock b) iblocks))
            (iblocks function))
    (list* (list function (start function)
                 (disassemble-lambda-list (lambda-list function)))
           iblocks)))

(defun disassemble-ir (ir)
  (check-type ir function)
  (let ((*seen* (make-set ir))
        (*work* (list ir))
        (*disassemble-nextv* 0)
        (*disassemble-vars* (make-hash-table :test #'eq)))
    (loop for work = (pop *work*)
          until (null work)
          collect (disassemble-function work))))
