(in-package #:cleavir-bir-transformations)

(defun copy-function (function)
  (%copy-function function nil (make-hash-table :test #'eq)))

(defun maybe-copy-of (thing map) (values (gethash thing map)))

(defun copy-of (thing map)
  (or (gethash thing map)
      (error "BUG: Not in map: ~a" thing)))

(defun finder (map) (lambda (thing) (copy-of thing map)))

(defun (setf copy-of) (copy thing map)
  (multiple-value-bind (old presentp) (gethash thing map)
    (when presentp
      (error "BUG: Tried to copy ~a twice: Old ~a new ~a"
             thing (gethash thing map) old copy)))
  (setf (gethash thing map) copy))

(defun copy-variable (variable stack map)
  (cond ((maybe-copy-of variable map))
        ((member (cleavir-bir:function variable) stack)
         (error "BUG: Function ~a is in the stack, but its variable ~a is not in the map"
                (cleavir-bir:function variable) variable))
        (t
         (setf (copy-of variable map)
               (make-instance 'cleavir-bir:variable
                 :name (cleavir-bir:name variable)
                 :extent (cleavir-bir:extent variable)
                 :use-status (cleavir-bir:use-status variable)
                 :ignore (cleavir-bir:ignore variable)
                 :rtype (cleavir-bir:rtype variable))))))

(defun variable-copier (stack map)
  (lambda (variable) (copy-variable variable stack map)))

(defun copy-argument (argument map)
  (setf (copy-of argument map)
        (make-instance 'cleavir-bir:argument
          :name (cleavir-bir:name argument)
          :rtype (cleavir-bir:rtype argument))))

(defun copy-lambda-list (lambda-list map)
  (loop for thing in lambda-list
        collect (cond ((member thing lambda-list-keywords) thing)
                      ((listp thing)
                       (if (= (length thing) 3)
                           (list (first thing)
                                 (copy-argument (second thing) map)
                                 (copy-argument (third thing) map))
                           (list (copy-argument (first thing) map)
                                 (copy-argument (second thing) map))))
                      (t (copy-argument thing map)))))

(defun %copy-function (function stack map)
  (let* ((module (cleavir-bir:module function))
         (copy (make-instance 'cleavir-bir:function
                 :name (cleavir-bir:name function)
                 :iblocks (cleavir-set:empty-set)
                 :docstring (cleavir-bir:docstring function)
                 :original-lambda-list (cleavir-bir:original-lambda-list
                                        function)
                 :origin (cleavir-bir:origin function)
                 :variables (cleavir-set:mapset
                             'cleavir-set:set
                             (variable-copier stack map)
                             (cleavir-bir:variables function))
                 :module module))
         (stack (cons function stack)))
    (cleavir-set:nadjoinf (cleavir-bir:functions module) copy)
    (setf (copy-of function map) copy)
    (setf (cleavir-bir:lambda-list copy)
          (copy-lambda-list (cleavir-bir:lambda-list function) map)
          ;; TODO
          (cleavir-bir:returni copy) :FIXME)
    (cleavir-bir:map-iblocks
     (lambda (ib)
       (let ((copy-ib (copy-iblock ib map copy)))
         (when (eq ib (cleavir-bir:start function))
           (setf (cleavir-bir:start copy) copy-ib))))
     function)
    (mapc
     (lambda (ib) (fill-iblock-copy ib stack map))
     (cleavir-bir::iblocks-forward-flow-order function))
    copy))

(defun copy-phi (datum map)
  (or (maybe-copy-of datum map)
      (setf (copy-of datum map)
            (make-instance 'cleavir-bir:phi
              :name (cleavir-bir:name datum)
              :iblock (copy-of (cleavir-bir:iblock datum) map)
              :rtype (cleavir-bir:rtype datum)))))

(defun phi-copier (map) (lambda (datum) (copy-phi datum map)))

(defun copy-iblock (iblock map function-copy)
  (let ((copy (make-instance 'cleavir-bir:iblock
                :name (cleavir-bir:name iblock) :function function-copy)))
    (setf (copy-of iblock map) copy
          (cleavir-bir:inputs copy)
          (mapcar (phi-copier map) (cleavir-bir:inputs iblock)))
    (cleavir-set:nadjoinf (cleavir-bir:iblocks function-copy) copy)
    copy))

(defun fill-iblock-copy (iblock stack map)
  (let ((copy (copy-of iblock map)))
    (setf (cleavir-bir:inputs copy)
          (mapcar (finder map) (cleavir-bir:inputs iblock))
          (cleavir-bir:dynamic-environment copy)
          (copy-of (cleavir-bir:dynamic-environment iblock) map))
    ;; Copy the instructions
    (cleavir-bir::map-iblock-instructions
     (lambda (inst)
       (let ((cinst (copy-instruction inst stack map))
             (old-pred (cleavir-bir:predecessor inst)))
         (setf (copy-of inst map) cinst)
         (if (null old-pred)
             (setf (cleavir-bir:start copy) cinst)
             (let ((new-pred (copy-of old-pred map)))
               (setf (cleavir-bir:successor new-pred) cinst
                     (cleavir-bir:predecessor cinst) new-pred)))))
     (cleavir-bir:start iblock))
    ;; Hook up terminator
    (let ((term (copy-of (cleavir-bir:end iblock) map)))
      (setf (cleavir-bir:end copy) term))
    copy))

(defgeneric copy-output (datum stack map))

(defmethod copy-output ((datum cleavir-bir:output) stack map)
  (declare (ignore stack))
  (setf (copy-of datum map)
        (make-instance 'cleavir-bir:output
          :name (cleavir-bir:name datum)
          :rtype (cleavir-bir:rtype datum))))
(defmethod copy-output ((datum cleavir-bir:phi) stack map)
  (declare (ignore stack))
  (copy-phi datum map))
(defmethod copy-output ((datum cleavir-bir:variable) stack map)
  (copy-variable datum stack map))

(defun output-copier (stack map)
  (lambda (datum) (copy-output datum stack map)))

;; Usually just needs to find the damn thing from a previous output
(defgeneric copy-input (datum map))
(defmethod copy-input ((datum cleavir-bir:datum) map)
  (copy-of datum map))
(defmethod copy-input ((datum cleavir-bir:load-time-value) map)
  (declare (ignore map))
  (make-instance 'cleavir-bir:load-time-value
    :name (cleavir-bir:name datum)
    :form (cleavir-bir:form datum)
    :read-only-p (cleavir-bir:read-only-p datum)
    :rtype (cleavir-bir:rtype datum)))

(defun input-copier (map)
  (lambda (datum) (copy-input datum map)))

(defgeneric clone-initargs (instruction stack map)
  (:method-combination append))

(defgeneric initialize-copy (copy)
  (:method ((instruction cleavir-bir:instruction)) instruction))

(defun copy-instruction (instruction stack map)
  (initialize-copy (apply #'make-instance (class-of instruction)
                          (clone-initargs instruction stack map))))

(defmethod initialize-copy :after ((copy cleavir-bir:terminator))
  (loop with ib = (cleavir-bir:iblock copy)
        for n in (cleavir-bir:next copy)
        do (cleavir-set:nadjoinf (cleavir-bir:predecessors n) ib)))

(defmethod initialize-copy :after ((term cleavir-bir:unwind))
  (cleavir-set:nadjoinf (cleavir-bir:unwinds
                         (cleavir-bir:catch term))
                        term)
  (cleavir-set:nadjoinf (cleavir-bir:entrances
                         (cleavir-bir:destination term))
                        (cleavir-bir:iblock term)))

(defmethod initialize-copy :after ((bind cleavir-bir:leti))
  (setf (cleavir-bir:binder bind) (first (cleavir-bir:outputs bind))))

(defmethod initialize-copy :after ((c cleavir-bir:catch))
  (cleavir-set:nadjoinf (cleavir-bir:catches (cleavir-bir:function c)) c))

(defmethod initialize-copy :after ((e cleavir-bir:enclose))
  (setf (cleavir-bir:enclose (cleavir-bir:code e)) e))

(defmethod clone-initargs append
    ((instruction cleavir-bir:instruction) stack map)
  (declare (ignore stack))
  (list
   :inputs (mapcar (input-copier map) (cleavir-bir:inputs instruction))
   :iblock (copy-of (cleavir-bir:iblock instruction) map)
   :policy (cleavir-bir:policy instruction)
   :origin (cleavir-bir:origin instruction)))

(defmethod clone-initargs append
    ((instruction cleavir-bir:operation) stack map)
  (list
   :outputs (mapcar (output-copier stack map)
                    (cleavir-bir:outputs instruction))))

(defmethod clone-initargs append
    ((instruction cleavir-bir:terminator) stack map)
  (declare (ignore stack))
  (list
   :next (mapcar (finder map) (cleavir-bir:next instruction))))

(defmethod clone-initargs append
    ((instruction cleavir-bir:enclose) stack map)
  (list
   :code (%copy-function (cleavir-bir:code instruction) stack map)))

(defmethod clone-initargs append
    ((instruction cleavir-bir:primop) stack map)
  (declare (ignore stack map))
  (list
   :info (cleavir-bir:info instruction)))

(defmethod clone-initargs append
    ((instruction cleavir-bir:abstract-call) stack map)
  (declare (ignore stack map))
  (list
   :attributes (cleavir-bir:attributes instruction)
   :transforms (cleavir-bir:transforms instruction)))

(defmethod clone-initargs append
    ((instruction cleavir-bir:alloca) stack map)
  (declare (ignore stack map))
  (list
   :rtype (cleavir-bir:rtype instruction)))

(defmethod clone-initargs append
    ((instruction cleavir-bir:unwind) stack map)
  (let* ((original-dest (cleavir-bir:destination instruction))
         (original-dest-function (cleavir-bir:function original-dest))
         (original-catch (cleavir-bir:catch instruction)))
    (if (member original-dest-function stack)
        (list :catch (copy-of original-catch map)
              :destination (copy-of original-dest map))
        (list :catch original-catch :destination original-dest))))

(defmethod clone-initargs append
    ((instruction cleavir-bir:jump) stack map)
  (declare (ignore stack map)))

(defmethod clone-initargs append
    ((instruction cleavir-bir:typeq-test) stack map)
  (declare (ignore stack map))
  (list :ctype (cleavir-bir:test-ctype instruction)))

(defmethod clone-initargs append
    ((instruction cleavir-bir:typew) stack map)
  (declare (ignore stack map))
  (list :ctype (cleavir-bir:ctype instruction)))

(defmethod clone-initargs append
    ((instruction cleavir-bir:case) stack map)
  (declare (ignore stack map))
  (list :comparees (cleavir-bir:comparees instruction)))

(defmethod clone-initargs append
    ((instruction cleavir-bir:cast) stack map)
  (declare (ignore stack map))
  (list :rtype (cleavir-bir:rtype instruction)))
