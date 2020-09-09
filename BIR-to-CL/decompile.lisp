(in-package #:cleavir-bir-to-cl)

(defgeneric decompile-function (function))
(defgeneric decompile-instruction (instruction))

(defvar *locals*)
(defvar *iblock-tags*)
(defvar *catch-tags*)

(defun find-value (value locals)
  (cond ((null locals) nil)
        ((gethash value (first locals)))
        (t (find-value value (rest locals)))))

(defun decompile-value (value)
  (or (find-value value *locals*)
      (setf (gethash value (first *locals*)) (gensym))))

(defun find-iblock (iblock)
  (or (cdr (assoc iblock *iblock-tags*))
      (error "BUG: No tag for iblock: ~a" iblock)))

(defmethod decompile-instruction ((inst cleavir-bir:enclose))
  (list `(setq ,(decompile-value inst)
               ,(decompile-function (cleavir-bir:code inst)))))

(defmethod decompile-instruction ((inst cleavir-bir:unreachable))
  (list `(error "BUG: Hit unreachable")))

(defmethod decompile-instruction ((inst cleavir-bir:readvar))
  (let ((var (first (cleavir-bir:inputs inst))))
    (if (typep (cleavir-bir:binder var) 'cleavir-bir:catch)
        nil ; special cased
        (list `(setq ,(decompile-value inst) ,(decompile-value var))))))

(defmethod decompile-instruction ((inst cleavir-bir:writevar))
  (let ((var (first (cleavir-bir:outputs inst))))
    (if (typep (cleavir-bir:binder var) 'cleavir-bir:catch)
        nil
        (list `(setq ,(decompile-value var)
                     ,(decompile-value (first (cleavir-bir:inputs inst))))))))

(defmethod decompile-instruction ((inst cleavir-bir:call))
  (list `(setq ,(decompile-value inst)
               (multiple-value-list
                (funcall
                 ,@(mapcar #'decompile-value (cleavir-bir:inputs inst)))))))

(defmethod decompile-instruction ((inst cleavir-bir:returni))
  (list `(return
           (values-list ,(decompile-value (first (cleavir-bir:inputs inst)))))))

(defmethod decompile-instruction ((inst cleavir-bir:catch))
  (list `(go ,(find-iblock (first (cleavir-bir:next inst))))))

(defmethod decompile-instruction ((inst cleavir-bir:unwind))
  (nreverse (list* `(go ,(find-iblock (cleavir-bir:destination inst)))
                   (loop for in in (rest (cleavir-bir:inputs inst))
                         for out in (cleavir-bir:outputs inst)
                         collect `(setq ,(decompile-value out)
                                        ,(decompile-value in))))))

(defmethod decompile-instruction ((inst cleavir-bir:jump))
  (list `(go ,(find-iblock (first (cleavir-bir:next inst))))))

(defmethod decompile-instruction ((inst cleavir-bir:eqi))
  (list `(if (eq ,@(mapcar #'decompile-value (cleavir-bir:inputs inst)))
             (go ,(find-iblock (first (cleavir-bir:next inst))))
             (go ,(find-iblock (second (cleavir-bir:next inst)))))))

(defmethod decompile-instruction ((inst cleavir-bir:fixed-to-multiple))
  (list `(setq ,(decompile-value inst)
               (list ,@(mapcar #'decompile-value (cleavir-bir:inputs inst))))))

(defmethod decompile-instruction ((inst cleavir-bir:multiple-to-fixed))
  (list `(multiple-value-setq
             (,@(mapcar #'decompile-value (cleavir-bir:outputs inst)))
           (values-list ,(decompile-value (first (cleavir-bir:inputs inst)))))))

(defun decompile-iblock (iblock)
  (loop for instruction = (cleavir-bir:start iblock)
          then (cleavir-bir:successor instruction)
        until (null instruction)
        nconc (decompile-instruction instruction)))

(defun decompile-lambda-list (lambda-list)
  (loop for item in lambda-list
        collect (cond ((member item lambda-list-keywords) item)
                      ((typep item 'cleavir-bir:argument)
                       (decompile-value item))
                      ((= (length item) 3)
                       `(,(first item)
                         ,(decompile-value (second item))
                         ,(decompile-value (third item))))
                      (t (mapcar #'decompile-value item)))))

(defmethod decompile-function ((function cleavir-bir:function))
  (let* ((*locals* (cons (make-hash-table :test #'eq) *locals*))
         (*iblock-tags* *iblock-tags*)
         (lambda-list
           (decompile-lambda-list (cleavir-bir:lambda-list function)))
         (body nil))
    (cleavir-bir:map-iblocks (lambda (ib)
                               ;; initialize phis
                               (mapc #'decompile-value (cleavir-bir:inputs ib))
                               ;; assign tag
                               (push (cons ib (gensym "TAG")) *iblock-tags*))
                             function)
    ;; generate iblocks
    (cleavir-bir:map-iblocks
     (lambda (ib)
       (setf body
             (nconc (list* (find-iblock ib) (decompile-iblock ib))
                    body)))
     function)
    ;; final body
    (let ((vars (loop for k being the hash-keys of (first *locals*)
                        using (hash-value v)
                      unless (typep k 'cleavir-bir:argument)
                        collect v)))
      `(lambda ,lambda-list
         (prog (,@vars)
            (go ,(find-iblock (cleavir-bir:start function)))
            ,@body)))))

(defun decompile (ir)
  (let ((*locals* nil) (*iblock-tags* nil))
    (decompile-function ir)))
