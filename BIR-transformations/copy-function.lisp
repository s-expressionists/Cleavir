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
        do (cond ((member thing lambda-list-keywords))
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
          (copy-lambda-list (cleavir-bir:lambda-list function) map))
    (cleavir-bir:map-iblocks
     (lambda (ib)
       (let ((copy-ib (copy-iblock ib map copy)))
         (setf (copy-of ib map) copy-ib)
         (cleavir-set:nadjoinf (cleavir-bir:iblocks copy) copy-ib)
         (when (eq ib (cleavir-bir:start function))
           (setf (cleavir-bir:start copy) copy-ib))
         (when (eq ib (cleavir-bir:end function))
           (setf (cleavir-bir:end copy) copy-ib))))
     function)
    (mapc
     (lambda (ib) (fill-iblock-copy ib stack map))
     (cleavir-bir::iblocks-forward-flow-order function))
    copy))

(defun copy-iblock (iblock map function)
  (let ((copy (make-instance 'cleavir-bir:iblock
                :inputs (mapcar (finder map) (cleavir-bir:inputs iblock))
                :function function)))
    (cleavir-set:nadjoinf (cleavir-bir:iblocks function) copy)
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
  (declare (ignore stack map))
  (make-instance 'cleavir-bir:output
    :name (cleavir-bir:name datum)
    :rtype (cleavir-bir:rtype datum)))
(defmethod copy-output ((datum cleavir-bir:phi) stack map)
  (declare (ignore stack))
  (make-instance 'cleavir-bir:phi
    :name (cleavir-bir:name datum)
    :iblock (copy-of (cleavir-bir:iblock datum) map)
    :rtype (cleavir-bir:rtype datum)))
(defmethod copy-output ((datum cleavir-bir:variable) stack map)
  (copy-variable datum stack map))

(defun output-copier (map) (lambda (datum) (copy-output datum map)))

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
        do (cleavir-set:nadjoinf (cleavir-bir:predecessors n) copy)))

(defmethod initialize-copy :after ((term cleavir-bir:unwind))
  (cleavir-set:nadjoinf (cleavir-bir:unwinds
                         (cleavir-bir:catch term))
                        term)
  (cleavir-set:nadjoinf (cleavir-bir:entrances
                         (cleavir-bir:destination term))
                        (cleavir-bir:iblock term)))

(defmethod clone-initargs append
    ((instruction cleavir-bir:instruction) stack map)
  (declare (ignore stack))
  (list
   :inputs (mapcar (finder map) (cleavir-bir:inputs instruction))
   :iblock (copy-of (cleavir-bir:iblock instruction) map)
   :policy (cleavir-bir:policy instruction)
   :origin (cleavir-bir:origin instruction)))

(defmethod clone-initargs append
    ((instruction cleavir-bir:operation) stack map)
  (declare (ignore stack))
  (list
   :outputs (mapcar (output-copier map) (cleavir-bir:outputs instruction))))

(defmethod clone-initargs append
    ((instruction cleavir-bir:terminator) stack map)
  (declare (ignore stack))
  (list
   :next (mapcar (finder map) (cleavir-bir:next instruction))))

(defmethod clone-initargs append
    ((instruction cleavir-bir:enclose) stack map)
  (declare (ignore stack map))
  (error "Copying an enclose is not implemented yet"))

(defmethod clone-initargs append
    ((instruction cleavir-bir:primop) stack map)
  (declare (ignore stack))
  (list
   :info (cleavir-bir:info instruction)))

(defmethod clone-initargs append
    ((instruction cleavir-bir:abstract-call) stack map)
  (list
   :attributes (cleavir-bir:attributes instruction)
   :transforms (cleavir-bir:transforms instruction)))

(defmethod clone-initargs append
    ((instruction cleavir-bir:alloca) stack map)
  (list
   :rtype (cleavir-bir:rtype instruction)))

(defmethod clone-initargs append
    ((instruction cleavir-bir:leti) stack map)
  (declare (ignore stack))
  (list
   :bindings (cleavir-set:mapset
              'cleavir-set:set
              (finder map) (cleavir-bir:bindings instruction))))

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
    ((instruction cleavir-bir:typeq) stack map)
  (declare (ignore stack map))
  (list :type-specifier (cleavir-bir:type-specifier instruction)))

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
