(in-package #:cleavir-ast-to-bir)

(defvar *variables*)
(defvar *block-info*)
(defvar *go-info*)
(defvar *function-info*)
(defvar *current-module*)

;;; KLUDGE: We need to write the following two functions this way
;;; because mutually referential functions can cause lexical-binds to
;;; occur after the name they bind is referenced.
(defun bind-variable (lexical-ast binder)
  (let ((variable (gethash lexical-ast *variables*)))
    (cond (variable
           ;; Tie the knot for mutually referential functions.
           (assert (null (cleavir-bir:binder variable)))
           (setf (cleavir-bir:binder variable) binder)
           variable)
          (t
           (setf (gethash lexical-ast *variables*)
                 (make-instance 'cleavir-bir:variable
                                :name (cleavir-ast:name lexical-ast)
                                :binder binder :rtype :object))))))

(defun find-variable (lexical-ast)
  (check-type lexical-ast cleavir-ast:lexical-ast)
  (or (gethash lexical-ast *variables*)
      ;; Normally this should never happen but mutually referential
      ;; functions create a circularity we must tie.
      (bind-variable lexical-ast nil)))

(defclass inserter ()
  ((%iblock :initarg :iblock :accessor iblock)
   (%insert-point :initarg :insert-point :accessor insert-point
                  ;; null means no instructions have been added yet.
                  :type (or null instruction))))

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

(defun make-iblock (inserter
                    &key (function (function inserter))
                      (dynamic-environment
                       (cleavir-bir:dynamic-environment (iblock inserter))))
  (let ((ib (make-instance 'cleavir-bir:iblock
              :function function :dynamic-environment dynamic-environment)))
    (cleavir-set:nadjoinf (cleavir-bir:iblocks function) ib)
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

;; Insert code or whatever else to adapt the results (from compile-ast) to match
;; the target. TARGET may be :multiple-values or a list of rvalues.
;; RESULTS must not be :no-return. Gotta handle that yourself.
;; Returns a list of data, suitable as instruction inputs.
(defun adapt (inserter results target)
  (assert (not (eq results :no-return)))
  (labels ((maybe-cast (ldatum rtype)
             (let ((ldrt (cleavir-bir:rtype ldatum)))
               (assert (not (eq ldrt :multiple-values)))
               (if (eq ldrt rtype)
                   ldatum
                   (insert inserter
                           (make-instance 'cleavir-bir:cast
                             :inputs (list ldatum) :rtype rtype)))))
           (maybe-cast-to-object (ldatum)
             (maybe-cast ldatum :object)))
    (if (eq target :multiple-values)
        (if (listp results)
            ;; a bunch of values were returned, so just ftm
            (list (insert inserter
                          (make-instance 'cleavir-bir:fixed-to-multiple
                            :inputs (mapcar #'maybe-cast-to-object results))))
            ;; multiple values were returned
            (list results))
        (if (listp results)
            ;; our target is a bunch of values, and we have a bunch of values
            ;; either nil fill or take what we need
            (let ((nresults (length results)) (ntarget (length target))
                  (shared (mapcar #'maybe-cast results target)))
              (if (>= nresults ntarget)
                  shared
                  (append shared
                          (loop repeat (- ntarget nresults)
                                collect (cleavir-bir:make-constant 'nil)))))
            ;; target is a bunch of values and result is multiple-values,
            ;; so mtf.
            (let ((outputs (loop repeat (length target)
                                 collect (make-instance 'cleavir-bir:output
                                           :rtype :object))))
              (insert inserter (make-instance 'cleavir-bir:multiple-to-fixed
                                 :inputs (list results)
                                 :outputs outputs))
              (mapcar #'maybe-cast outputs target))))))

(defgeneric compile-function (ast system))

(defun compile-toplevel (ast system)
  (let ((*variables* (make-hash-table :test #'eq))
        (*block-info* (make-hash-table :test #'eq))
        (*go-info* (make-hash-table :test #'eq))
        (*function-info* (make-hash-table :test #'eq))
        (*current-module* (make-module)))
    (compile-function ast system)))

;;; Returns a list of data, or :no-return, or one datum (representing mvalues).
(defgeneric compile-ast (ast inserter system)
  (:method :around ((ast cleavir-ast:ast) inserter system)
    (declare (ignore inserter system))
    (let ((cleavir-bir:*origin* (cleavir-ast:origin ast))
          (cleavir-bir:*policy* (cleavir-ast:policy ast)))
      (call-next-method))))

(defgeneric compile-test-ast (ast inserter system)
  (:method :around ((ast cleavir-ast:ast) inserter system)
    (declare (ignore system))
    (let ((cleavir-bir:*origin* (cleavir-ast:origin ast))
          (cleavir-bir:*policy* (cleavir-ast:policy ast)))
      (call-next-method))))

(defmacro with-compiled-ast ((name ast inserter system
                              &optional (target ''(:object)))
                             &body body)
  (let ((gast (gensym "AST")) (gtarget (gensym "TARGET"))
        (ginserter (gensym "INSERTER")) (gsystem (gensym "SYSTEM")))
    `(let ((,gast ,ast) (,ginserter ,inserter) (,gsystem ,system)
           (,gtarget ,target))
       (let ((,name (compile-ast ,gast ,ginserter ,gsystem)))
         (if (eq ,name :no-return)
             ,name
             (let ((,name (adapt ,ginserter ,name ,gtarget)))
               ,@body))))))

(defmacro with-compiled-asts ((name (&rest asts) inserter system
                               (&rest targets))
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
                          for target in targets
                          for c = `(compile-ast ,gast ,ginserter ,gsystem)
                          collect `(let ((temp ,c))
                                     (if (eq temp :no-return)
                                         (return-from ,bname temp)
                                         (first
                                          (adapt ,ginserter temp
                                                 '(,target)))))))))
           ,@body)))))

(defun compile-arguments (arg-asts inserter system
                          &optional (target '(:object)))
  (loop for arg-ast in arg-asts
        for rv = (compile-ast arg-ast inserter system)
        if (eq rv :no-return)
          return rv
        else collect (adapt inserter rv target)))

(defmacro with-compiled-arguments ((name asts inserter system
                                    &optional (target ''(:object)))
                                   &body body)
  (let ((gasts (gensym "ASTS")) (ginserter (gensym "INSERTER"))
        (gsystem (gensym "SYSTEM")) (gtarget (gensym "TARGET")))
    `(let ((,gasts ,asts) (,ginserter ,inserter)
           (,gsystem ,system) (,gtarget ,target))
       (let ((,name (compile-arguments ,gasts ,ginserter ,gsystem ,gtarget)))
         (if (eq ,name :no-return)
             ,name
             (progn ,@body))))))

(defun compile-sequence-for-effect (asts inserter system)
  (loop for sub in asts
        for rv = (compile-ast sub inserter system)
        when (eq rv :no-return)
          return nil
        finally (return t)))
