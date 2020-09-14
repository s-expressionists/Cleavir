(in-package #:cleavir-bir)

(defvar *seen*)
(defvar *work*)

(defun maybe-add-disassemble-work (function)
  (check-type function function)
  (unless (cleavir-set:presentp function *seen*)
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

(defun dis-iblock (iblock)
  (or (gethash iblock *disassemble-vars*)
      (setf (gethash iblock *disassemble-vars*) (gensym "tag"))))

(defun dis-var (variable)
  (or (gethash variable *disassemble-vars*)
      (setf (gethash variable *disassemble-vars*)
            (prog1 (make-symbol (write-to-string *disassemble-nextv*))
              (incf *disassemble-nextv*)))))

(defgeneric dis-label (instruction))
(defmethod dis-label ((instruction instruction))
  (class-name (class-of instruction)))

(defgeneric disassemble-datum (value))

(defmethod disassemble-datum ((value linear-datum)) (dis-id value))
(defmethod disassemble-datum ((value constant)) `',(constant-value value))
(defmethod disassemble-datum ((value load-time-value))
  (if (and (read-only-p value) (constantp (form value)))
      `',(eval (form value))
      `(cl:load-time-value ,(form value) ,(read-only-p value))))
(defmethod disassemble-datum ((value variable)) (dis-var value))

(defgeneric disassemble-instruction (instruction))

(defmethod disassemble-instruction ((inst operation))
  (let ((outs (outputs inst))
        (base
          `(,(dis-label inst) ,@(mapcar #'disassemble-datum (inputs inst)))))
    (if (null outs)
        base
        `(:= ,(mapcar #'disassemble-datum (outputs inst)) ,base))))

(defmethod disassemble-instruction ((inst computation))
  `(:= ,(disassemble-datum inst)
       (,(dis-label inst) ,@(mapcar #'disassemble-datum (inputs inst)))))

(defmethod disassemble-instruction ((inst enclose))
  (maybe-add-disassemble-work (code inst))
  `(:= ,(disassemble-datum inst)
       (,(dis-label inst)
        ,(code inst)
        ,(mapcar #'dis-var (cleavir-set:set-to-list (variables inst)))
        ,@(mapcar #'disassemble-datum (inputs inst)))))

(defmethod disassemble-instruction ((inst catch))
  `(:= ,(disassemble-datum inst)
       (,(dis-label inst)
        ,@(mapcar #'disassemble-datum (inputs inst))
        ,@(mapcar #'dis-iblock (next inst)))))

(defmethod disassemble-instruction ((inst unwind))
  `(,(dis-label inst)
    ,@(mapcar #'disassemble-datum (inputs inst))
    :->
    ,(dis-iblock (destination inst))))

(defmethod disassemble-instruction ((inst jump))
  `(,(dis-label inst) ,@(mapcar #'disassemble-datum (inputs inst))
    ,(dis-iblock (first (next inst)))))

(defmethod disassemble-instruction ((inst leti))
  `(,(dis-label inst) ,(mapcar #'dis-var (cleavir-set:set-to-list (bindings inst)))
    ,(dis-iblock (first (next inst)))))

(defmethod disassemble-instruction ((inst eqi))
  `(,(dis-label inst) ,@(mapcar #'disassemble-datum (inputs inst))
    ,@(mapcar #'dis-iblock (next inst))))

(defun disassemble-iblock (iblock)
  (check-type iblock iblock)
  (let ((insts nil))
    (map-iblock-instructions
     (lambda (i) (push (disassemble-instruction i) insts))
     (start iblock))
    (list* (list* (dis-iblock iblock)
                  (mapcar #'disassemble-datum (inputs iblock)))
           (nreverse insts))))

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

(defun disassemble-function (function)
  (check-type function function)
  (refresh-iblocks function)
  (let ((iblocks nil)
        (*disassemble-ids* (make-hash-table :test #'eq))
        (*disassemble-nextn* 0))
    (cleavir-set:doset (b (iblocks function))
      (push (disassemble-iblock b) iblocks))
    (list* (list function (dis-iblock (start function))
                 (disassemble-lambda-list (lambda-list function)))
           iblocks)))

(defun disassemble (ir)
  (check-type ir function)
  (let ((*seen* (cleavir-set:make-set ir))
        (*work* (list ir))
        (*disassemble-nextv* 0)
        (*disassemble-vars* (make-hash-table :test #'eq)))
    (loop for work = (pop *work*)
          until (null work)
          collect (disassemble-function work))))
