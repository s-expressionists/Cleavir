(in-package #:cleavir-ast-to-bir)

(defvar *variables*)
(defvar *block-info*)
(defvar *go-info*)
(defvar *current-module*)

;;; KLUDGE: We need to write the following two functions this way
;;; because mutually referential functions can cause lexical-binds to
;;; occur after the name they bind is referenced.
(defun bind-variable (lexical-variable ignore)
  (let ((variable (gethash lexical-variable *variables*)))
    (cond (variable
           ;; Tie the knot for mutually referential functions.
           (assert (not (slot-boundp variable 'cleavir-bir::%binder))
                   ()
                   "Cannot bind variable more than once.")
           variable)
          (t
           (setf (gethash lexical-variable *variables*)
                 (make-instance 'cleavir-bir:variable
                                :name (cleavir-ast:name lexical-variable)
                                :ignore ignore))))))

(defun find-variable (lexical-variable)
  (check-type lexical-variable cleavir-ast:lexical-variable)
  (or (gethash lexical-variable *variables*)
      ;; Normally this should never happen but mutually referential
      ;; functions create a circularity we must tie.
      (bind-variable lexical-variable nil)))

(defclass inserter ()
  ((%iblock :initarg :iblock :accessor iblock)
   (%insert-point :initarg :insert-point :accessor insert-point
                  ;; null means no instructions have been added yet.
                  :type (or null cleavir-bir:instruction))))

(defun function (inserter) (cleavir-bir:function (iblock inserter)))
(defun dynamic-environment (inserter)
  (cleavir-bir:dynamic-environment (iblock inserter)))

;; Put the insert point at the last instruction in the block.
;; (The terminator shouldn't be set yet.)
(defun proceed (inserter iblock)
  (setf (iblock inserter) iblock
        (insert-point inserter)
        (if (cleavir-bir:iblock-started-p iblock)
            (loop for inst = (cleavir-bir:start iblock) then succ
                  for succ = (cleavir-bir:successor inst)
                  when (typep inst 'cleavir-bir:terminator)
                    do (error "BUG: Tried to PROCEED a terminated block")
                  when (null succ)
                    return inst)
            nil)))

;; PROCEED when the block is new and has no instructions.
(defun begin (inserter iblock)
  (setf (iblock inserter) iblock (insert-point inserter) nil))

(defun insert (inserter instruction)
  (let ((ip (insert-point inserter))
        (ib (iblock inserter)))
    (if (null ip)
        (setf (cleavir-bir:start ib) instruction)
        (setf (cleavir-bir:predecessor instruction) ip
              (cleavir-bir:successor ip) instruction))
    (setf (cleavir-bir:iblock instruction) ib
          (cleavir-bir:successor instruction) nil
          (insert-point inserter) instruction))
  instruction)

(defun terminate (inserter terminator)
  (let ((ib (iblock inserter)))
    (loop for next in (cleavir-bir:next terminator)
          do (cleavir-set:nadjoinf (cleavir-bir:predecessors next) ib))
    (let ((ip (insert-point inserter)))
      (if (null ip)
          (setf (cleavir-bir:start ib) terminator)
          (setf (cleavir-bir:successor ip) terminator))
      (setf (cleavir-bir:predecessor terminator) ip))
    (setf (cleavir-bir:end ib) terminator
          (cleavir-bir:iblock terminator) ib))
  terminator)

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
                       (cleavir-bir:dynamic-environment (iblock inserter)))
                      name)
  (let ((ib (make-instance 'cleavir-bir:iblock
              :name name
              :function function :dynamic-environment dynamic-environment)))
    (cleavir-set:nadjoinf (cleavir-bir:scope dynamic-environment) ib)
    ib))

(defun make-module ()
  (make-instance 'cleavir-bir:module))

(defun adjoin-variable (inserter variable)
  (check-type inserter inserter)
  (check-type variable cleavir-bir:variable)
  (cleavir-set:nadjoinf (cleavir-bir:variables (function inserter))
                        variable)
  (values))

(defgeneric compile-function (ast system))

(defun compile-toplevel (ast system)
  (let ((*variables* (make-hash-table :test #'eq))
        (*block-info* (make-hash-table :test #'eq))
        (*go-info* (make-hash-table :test #'eq))
        (*current-module* (make-module))
        (cleavir-bir:*top-ctype* (cleavir-ctype:top system))
        (cleavir-bir:*top-function-ctype* (cleavir-ctype:function-top system)))
    (compile-function ast system)))

;;; Returns a list of data, or :no-return, or one datum (representing mvalues).
(defgeneric compile-ast (ast inserter system)
  (:method :around ((ast cleavir-ast:ast) inserter system)
    (declare (ignore inserter system))
    (let ((cleavir-bir:*origin* (cleavir-ast:origin ast))
          (cleavir-bir:*policy* (cleavir-ast:policy ast))
          (result (call-next-method)))
      (assert (or (listp result) (eq result :no-value) (eq result :no-return)))
      result)))

(defgeneric compile-test-ast (ast inserter system)
  (:method :around ((ast cleavir-ast:ast) inserter system)
    (declare (ignore inserter system))
    (let ((cleavir-bir:*origin* (cleavir-ast:origin ast))
          (cleavir-bir:*policy* (cleavir-ast:policy ast)))
      (call-next-method))))

(defmacro with-compiled-ast ((name ast inserter system) &body body)
  (let ((gast (gensym "AST"))
        (ginserter (gensym "INSERTER")) (gsystem (gensym "SYSTEM")))
    `(let ((,gast ,ast) (,ginserter ,inserter) (,gsystem ,system))
       (let ((,name (compile-ast ,gast ,ginserter ,gsystem)))
         (if (eq ,name :no-return)
             ,name
             (progn ,@body))))))

(defmacro with-compiled-asts ((name (&rest asts) inserter system)
                              &body body)
  (let ((gasts (loop repeat (length asts) collect (gensym "AST")))
        (bname (gensym "WITH-COMPILED-ASTS"))
        (ginserter (gensym "INSERTER")) (gsystem (gensym "SYSTEM")))
    `(block ,bname
       (let (,@(mapcar #'list gasts asts)
             (,ginserter ,inserter) (,gsystem ,system))
         (let ((,name
                 (list
                  ,@(loop for gast in gasts
                          for c = `(compile-ast ,gast ,ginserter ,gsystem)
                          collect `(let ((temp ,c))
                                     (if (eq temp :no-return)
                                         (return-from ,bname temp)
                                         (first temp)))))))
           ,@body)))))

(defun compile-arguments (arg-asts inserter system)
  (loop for arg-ast in arg-asts
        for rv = (compile-ast arg-ast inserter system)
        if (eq rv :no-return)
          return rv
        else collect (first rv)))

(defmacro with-compiled-arguments ((name asts inserter system)
                                   &body body)
  (let ((gasts (gensym "ASTS")) (ginserter (gensym "INSERTER"))
        (gsystem (gensym "SYSTEM")))
    `(let ((,gasts ,asts) (,ginserter ,inserter) (,gsystem ,system))
       (let ((,name (compile-arguments ,gasts ,ginserter ,gsystem)))
         (if (eq ,name :no-return)
             ,name
             (progn ,@body))))))

(defun compile-sequence-for-effect (asts inserter system)
  (loop for sub in asts
        for rv = (compile-ast sub inserter system)
        when (eq rv :no-return)
          return nil
        finally (return t)))
