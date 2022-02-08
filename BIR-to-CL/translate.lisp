(in-package #:cleavir-bir-to-cl)

(defvar *generating-function*)
(defvar *generating-block*)
(defvar *function-names*)
(defvar *datum-names*)
(defvar *variable-names*)
(defvar *iblock-names*)

(defun name-thing (thing)
  (gensym (write-to-string (bir:name thing) :escape nil)))

(defun translate (module)
  (let ((*function-names* (make-hash-table))
        (*variable-names* (make-hash-table))
        (*iblock-names* (make-hash-table))
        entry-point) ; FIXME: This concept is presently vague in Cleavir.
    (bir:do-functions (function module)
      (setf (gethash function *function-names*) (name-thing function))
      (when (and (not (bir:enclose function))
                 (set:empty-set-p (bir:local-calls function)))
        (setf entry-point function)))
    `(lambda () ; TODO: linkage
       (labels (,@(set:mapset
                      'list
                    (lambda (function)
                      (destructuring-bind (lambda lambda-list &rest body)
                          (translate-main-function function)
                        (declare (ignore lambda))
                        `(,(gethash function *function-names*)
                          ,lambda-list ,@body)))
                    (bir:functions module)))
         ,(translate-xep-function entry-point)))))

(defun bind-lambda-list (lambda-list def)
  (let ((result-args nil)
        (result-ll nil))
    (bir:map-lambda-list
     (lambda (state item index)
       (declare (ignore state index))
       (cond ((member item lambda-list-keywords) (push item result-ll))
             ((atom item)
              (let ((d (funcall def item)))
                (push d result-args) (push d result-ll)))
             ((= (length item) 2)
              (let ((dv (funcall def (first item)))
                    (dp (funcall def (second item))))
                (push dv result-args) (push dp result-args)
                (push `(,dp nil ,dv) result-ll)))
             ((= (length item) 3)
              (let ((key (first item))
                    (dv (funcall def (second item)))
                    (dp (funcall def (third item))))
                (push dv result-args) (push dp result-args)
                (push `((,key ,dp) nil ,dv) result-ll)))))
     lambda-list)
    (values (nreverse result-args) (nreverse result-ll))))

(defun local-names (hash)
  (loop for datum being the hash-keys of hash
          using (hash-value name)
        if (typep datum 'bir:argument)
          collect `(,name (list ,name))
        else
          collect name))

(defun local-variables (function hash)
  (loop for var being the hash-keys of hash
          using (hash-value name)
        if (eq function (bir:function (bir:binder var)))
          collect name))

(defun translate-main-function (function)
  (let* ((*generating-function* nil)
         (*datum-names* (make-hash-table :test #'eq))
         (lambda-list
           (bind-lambda-list (bir:lambda-list function) #'def)))
    (bir:map-iblocks #'translate-iblock function)
    `(lambda (,@lambda-list)
       (prog (,@(local-names *datum-names*)
              ,@(local-variables function *variable-names*))
          ,@*generating-function*))))

(defun translate-xep-function (function)
  (multiple-value-bind (args ll)
      (bind-lambda-list (bir:lambda-list function) #'name-thing)
    `(lambda (,@ll)
       (funcall #',(gethash function *function-names*) ,@args))))

(defun gen-ib (iblock-code)
  (setf *generating-function* (nconc *generating-function* iblock-code)))
  
(defun translate-iblock (iblock)
  (let ((*generating-block* nil)
        (name (or (gethash iblock *iblock-names*)
                  (setf (gethash iblock *iblock-names*) (name-thing iblock)))))
    (bir:map-iblock-instructions #'translate-instruction iblock)
    (gen-ib `(,name ,@(nreverse *generating-block*)))))

(defun gen (form) (push form *generating-block*))

(defun bind-variable (variable)
  (setf (gethash variable *variable-names*) (name-thing variable)))

(defun variable-out (value variable)
  (multiple-value-bind (name presentp) (gethash variable *variable-names*)
    (unless presentp (error "Missing name for ~a" variable))
    (gen `(setf ,name (values (values-list ,value))))))

(defun variable-in (variable)
  (multiple-value-bind (name presentp) (gethash variable *variable-names*)
    (unless presentp (error "Missing name for ~a" variable))
    `(list ,name)))
    
(defun def (datum)
  (when (nth-value 1 (gethash datum *datum-names*))
    (error "Double OUT for ~a" datum))
  (setf (gethash datum *datum-names*) (name-thing datum)))
    
(defun out (value datum)
  (gen `(setf ,(def datum) ,value)))

(defun in (datum)
  (multiple-value-bind (name presentp) (gethash datum *datum-names*)
    (unless presentp (error "Missing name for ~a" datum))
    name))

(defgeneric translate-instruction (instruction))

(defmethod translate-instruction ((instruction bir:unreachable))
  (gen `(error "BUG: Hit unreachable")))

(defmethod translate-instruction :before ((instruction bir:leti))
  (bind-variable (bir:output instruction)))

(defmethod translate-instruction ((instruction bir:writevar))
  (variable-out (in (bir:input instruction)) (bir:output instruction)))

(defmethod translate-instruction ((instruction bir:readvar))
  (out (variable-in (bir:input instruction)) (bir:output instruction)))

(defmethod translate-instruction ((instruction bir:constant-reference))
  (out `(list ',(bir:constant-value (bir:input instruction)))
       (bir:output instruction)))

(defgeneric translate-primop (name instruction))

(defmethod translate-primop ((name (eql 'fdefinition)) inst)
  (let ((sym (in (first (bir:inputs inst))))
        (out (first (bir:outputs inst))))
    (out `(list (fdefinition (values (values-list ,sym)))) out)))

(defmethod translate-instruction ((instruction bir:primop))
  (translate-primop (cleavir-primop-info:name (bir:info instruction))
                    instruction))

(defmethod translate-instruction ((instruction bir:call))
  (out `(multiple-value-list
         (funcall ,@(loop for inp in (bir:inputs instruction)
                          collect `(values (values-list ,(in inp))))))
       (bir:output instruction)))

(defmethod translate-instruction ((instruction bir:returni))
  (gen `(return (values-list ,(in (bir:input instruction))))))
