(in-package #:cleavir-cst-to-bir)

(defvar *current-module*)

(defclass inserter ()
  ((%iblock :initarg :iblock :accessor iblock)
   (%insert-point :initarg :insert-point :accessor insert-point
                  ;; null means no instructions have been added yet.
                  :type (or null bir:instruction))))

(defun function (inserter) (bir:function (iblock inserter)))
(defun dynamic-environment (inserter)
  (bir:dynamic-environment (iblock inserter)))

;; Put the insert point at the last instruction in the block.
;; (The terminator shouldn't be set yet.)
(defun proceed (inserter iblock)
  (setf (iblock inserter) iblock
        (insert-point inserter)
        (if (bir:iblock-started-p iblock)
            (loop for inst = (bir:start iblock) then succ
                  for succ = (bir:successor inst)
                  when (typep inst 'bir:terminator)
                    do (error "BUG: Tried to PROCEED a terminated block")
                  when (null succ)
                    return inst)
            nil)))

;; PROCEED when the block is new and has no instructions.
(defun begin (inserter iblock)
  (setf (iblock inserter) iblock (insert-point inserter) nil))

(defun %insert (inserter instruction)
  (let ((ip (insert-point inserter))
        (ib (iblock inserter)))
    (if (null ip)
        (setf (bir:start ib) instruction)
        (setf (bir:predecessor instruction) ip
              (bir:successor ip) instruction))
    (setf (bir:iblock instruction) ib
          (bir:successor instruction) nil
          (insert-point inserter) instruction))
  instruction)

;;; Create an instruction from an "instruction designator", which is
;;; kind of like a condition designator - either an instruction or
;;; arguments to make-instance.
(defun instruction (datum &rest initargs)
  (etypecase datum
    (bir:instruction datum)
    (symbol (apply #'make-instance datum initargs))))
;;; Take advantage of implementation optimizations on make-instance:
;;; for (instruction 'classname ...), expand to make-instance.
(define-compiler-macro instruction (&whole whole datum &rest initargs)
  ;; CONSTANTP would be better, but without CONSTANT-FORM-VALUE it is
  ;; slightly dicey.
  (if (and (consp datum)
           (eq (first datum) 'quote)
           (consp (cdr datum))
           (symbolp (second datum))
           (null (cddr datum)))
      `(make-instance ,datum ,@initargs)
      whole))

(defun insert (inserter datum &rest initargs)
  (%insert inserter (apply #'instruction datum initargs)))
(define-compiler-macro insert (inserter datum &rest initargs)
  `(%insert ,inserter (instruction ,datum ,@initargs)))

(defun %terminate (inserter terminator)
  (let ((ib (iblock inserter)))
    (loop for next in (bir:next terminator)
          do (set:nadjoinf (bir:predecessors next) ib))
    (let ((ip (insert-point inserter)))
      (if (null ip)
          (setf (bir:start ib) terminator)
          (setf (bir:successor ip) terminator))
      (setf (bir:predecessor terminator) ip))
    (setf (bir:end ib) terminator
          (bir:iblock terminator) ib))
  terminator)

(defun terminate (inserter datum &rest initargs)
  (%terminate inserter (apply #'instruction datum initargs)))
(define-compiler-macro terminate (inserter datum &rest initargs)
  `(%terminate ,inserter (instruction ,datum ,@initargs)))

;;; internal helper
(defun symbolicate (&rest components)
  (let* ((strings (mapcar #'string components))
         (length (reduce #'+ strings :key #'length))
         (name (make-array length :element-type 'character)))
    (let ((index 0))
      (dolist (string strings (make-symbol name))
        (replace name string :start1 index)
        (incf index (length string))))))

(defun make-iblock (inserter
                    &key (function (function inserter))
                      (dynamic-environment
                       (bir:dynamic-environment (iblock inserter)))
                      name)
  (let ((ib (make-instance 'bir:iblock
              :name name
              :function function :dynamic-environment dynamic-environment)))
    (set:nadjoinf (bir:scope dynamic-environment) ib)
    ib))

(defun make-module ()
  (make-instance 'bir:module))

(defun adjoin-variable (inserter variable)
  (check-type inserter inserter)
  (check-type variable bir:variable)
  (set:nadjoinf (bir:variables (function inserter)) variable)
  (values))

#+(or)
(defgeneric compile-function (ast system)
  (:method :around ((ast ast:function-ast) system)
    (declare (ignore system))
    (let ((bir:*origin* (ast:origin ast))
          (bir:*policy* (ast:policy ast)))
      (call-next-method))))

#+(or)
(defun compile-into-module (ast module system)
  (let ((*variables* (make-hash-table :test #'eq))
        (*block-info* (make-hash-table :test #'eq))
        (*go-info* (make-hash-table :test #'eq))
        (*current-module* module)
        (bir:*top-ctype* (ctype:coerce-to-values (ctype:top system) system)))
    (compile-function ast system)))

#+(or)
(defun compile-toplevel (ast system)
  (compile-into-module ast (make-module) system))

#+(or)
(defgeneric compile-ast (ast inserter system)
  (:method :around ((ast ast:ast) inserter system)
    (declare (ignore inserter system))
    (let ((bir:*origin* (ast:origin ast))
          (bir:*policy* (ast:policy ast))
          (result (call-next-method)))
      (assert (or (listp result) (eq result :no-value) (eq result :no-return)))
      result)))

(defmacro with-compiled-cst ((name cst inserter env system) &body body)
  (let ((gcst (gensym "CST"))
        (ginserter (gensym "INSERTER"))
        (genv (gensym "ENVIRONMENT")) (gsystem (gensym "SYSTEM")))
    `(let ((,gcst ,cst) (,ginserter ,inserter)
           (,genv ,env) (,gsystem ,system))
       (let ((,name (convert ,gcst ,ginserter ,genv ,gsystem)))
         (if (eq ,name :no-return)
             ,name
             (progn ,@body))))))

(defmacro with-compiled-csts ((name (&rest csts) inserter env system)
                              &body body)
  (let ((gcsts (loop repeat (length csts) collect (gensym "CST")))
        (bname (gensym "WITH-COMPILED-ASTS"))
        (ginserter (gensym "INSERTER")) (genv (gensym "ENVIRONMENT"))
        (gsystem (gensym "SYSTEM")))
    `(block ,bname
       (let (,@(mapcar #'list gcsts csts)
             (,ginserter ,inserter) (,genv ,env) (,gsystem ,system))
         (let ((,name
                 (list
                  ,@(loop for gcst in gcsts
                          for c = `(convert ,gcst ,ginserter ,genv ,gsystem)
                          collect `(let ((temp ,c))
                                     (if (eq temp :no-return)
                                         (return-from ,bname temp)
                                         (first temp)))))))
           ,@body)))))

(defun compile-arguments (arguments-cst inserter env system)
  (loop for cur = arguments-cst then (cst:rest cur)
        until (cst:null cur)
        collect (let ((rv (convert (cst:first cur) inserter env system)))
                  (if (eq rv :no-return)
                      (return :no-return)
                      (first rv)))))

(defmacro with-compiled-arguments ((name args inserter env system)
                                   &body body)
  (let ((gargs (gensym "ARGS"))
        (ginserter (gensym "INSERTER"))
        (genv (gensym "ENVIRONMENT")) (gsystem (gensym "SYSTEM")))
    `(let ((,gargs ,args) (,ginserter ,inserter)
           (,genv ,env) (,gsystem ,system))
       (let ((,name (compile-arguments ,gargs ,ginserter ,genv ,gsystem)))
         (if (eq ,name :no-return)
             ,name
             (progn ,@body))))))
