(in-package #:cleavir-ast-to-bir)

(defvar *variables*)
(defvar *block-info*)
(defvar *go-info*)
(defvar *inlined-at*)
(defvar *current-module*)

;;; KLUDGE: We need to write the following two functions this way
;;; because mutually referential functions can cause lexical-binds to
;;; occur after the name they bind is referenced.
(defun bind-variable (lexical-variable ignore)
  (let ((variable (gethash lexical-variable *variables*)))
    (cond (variable
           ;; Tie the knot for mutually referential functions.
           (assert (not (slot-boundp variable 'bir::%binder))
                   ()
                   "Cannot bind variable more than once.")
           variable)
          (t
           (setf (gethash lexical-variable *variables*)
                 (make-instance 'bir:variable
                                :name (ast:name lexical-variable)
                                :ignore ignore))))))

(defun find-variable (lexical-variable)
  (check-type lexical-variable ast:lexical-variable)
  (or (gethash lexical-variable *variables*)
      ;; Normally this should never happen but mutually referential
      ;; functions create a circularity we must tie.
      (bind-variable lexical-variable nil)))

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

;;; Modify a source position to indicate information about inlining.
;;; ORIGIN is the origin for some part of the inlined body, and INLINED-AT
;;; is the origin of the callee.
;;; The nature of ORIGINs is still client-defined.
(defgeneric inline-origin (origin inlined-at system)
  (:method (origin (inlined-at null) system)
    (declare (ignore system))
    origin)
  (:method (origin inlined-at system)
    (declare (ignore inlined-at system))
    origin))

(defgeneric compile-function (ast system)
  (:method :around ((ast ast:function-ast) system)
    (let ((bir:*origin* (inline-origin (ast:origin ast) *inlined-at* system))
          (bir:*policy* (ast:policy ast)))
      (call-next-method))))

(defun compile-into-module (ast module system)
  (let ((*variables* (make-hash-table :test #'eq))
        (*block-info* (make-hash-table :test #'eq))
        (*go-info* (make-hash-table :test #'eq))
        (*inlined-at* nil)
        (*current-module* module)
        (bir:*top-ctype* (ctype:values-top system)))
    (compile-function ast system)))

(defun compile-toplevel (ast system)
  (compile-into-module ast (make-module) system))

;;; Returns a list of data, or :no-return, or one datum (representing mvalues).
(defgeneric compile-ast (ast inserter system)
  (:method :around ((ast ast:ast) inserter system)
    (declare (ignore inserter))
    (let* ((bir:*origin* (inline-origin (ast:origin ast) *inlined-at* system))
           (bir:*policy* (ast:policy ast))
           (result (call-next-method)))
      (assert (or (listp result) (eq result :no-value) (eq result :no-return)))
      result)))

(defgeneric compile-test-ast (ast inserter system)
  (:method :around ((ast ast:ast) inserter system)
    (declare (ignore inserter))
    (let ((bir:*origin* (inline-origin (ast:origin ast) *inlined-at* system))
          (bir:*policy* (ast:policy ast)))
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
