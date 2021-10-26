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
             thing old copy)))
  (setf (gethash thing map) copy))

(defun copy-variable (variable stack map)
  (cond ((maybe-copy-of variable map))
        ((member (bir:function variable) stack)
         (error "BUG: Function ~a is in the stack, but its variable ~a is not in the map"
                (bir:function variable) variable))
        (t
         (setf (copy-of variable map)
               (make-instance 'bir:variable
                 :name (bir:name variable)
                 :extent (bir:extent variable)
                 :use-status (bir:use-status variable)
                 :ignore (bir:ignore variable))))))

(defun variable-copier (stack map)
  (lambda (variable) (copy-variable variable stack map)))

(defun copy-argument (argument map)
  (setf (copy-of argument map)
        (make-instance 'bir:argument
          :name (bir:name argument))))

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
  (let* ((module (bir:module function))
         (copy (make-instance 'bir:function
                 :name (bir:name function)
                 :iblocks (set:empty-set)
                 :docstring (bir:docstring function)
                 :original-lambda-list (bir:original-lambda-list
                                        function)
                 :origin (bir:origin function)
                 :variables (set:mapset
                             'set:set
                             (variable-copier stack map)
                             (bir:variables function))
                 :module module))
         (stack (cons function stack)))
    (set:nadjoinf (bir:functions module) copy)
    (setf (copy-of function map) copy)
    (setf (bir:lambda-list copy)
          (copy-lambda-list (bir:lambda-list function) map)
          ;; TODO
          (bir:returni copy) :FIXME)
    (bir:map-iblocks
     (lambda (ib)
       (let ((copy-ib (copy-iblock ib map copy)))
         (when (eq ib (bir:start function))
           (setf (bir:start copy) copy-ib))))
     function)
    (bir:do-iblocks (ib function)
      (fill-iblock-copy ib stack map))
    copy))

(defun copy-phi (datum map)
  (or (maybe-copy-of datum map)
      (setf (copy-of datum map)
            (make-instance 'bir:phi
              :name (bir:name datum)
              :iblock (copy-of (bir:iblock datum) map)))))

(defun phi-copier (map) (lambda (datum) (copy-phi datum map)))

(defun copy-iblock (iblock map function-copy)
  (let ((copy (make-instance 'bir:iblock
                :name (bir:name iblock) :function function-copy)))
    (setf (copy-of iblock map) copy
          (bir:inputs copy)
          (mapcar (phi-copier map) (bir:inputs iblock)))
    ;; FIXME:
    #+(or)
    (set:nadjoinf (bir:iblocks function-copy) copy)
    copy))

(defun fill-iblock-copy (iblock stack map)
  (let ((copy (copy-of iblock map)))
    (setf (bir:inputs copy)
          (mapcar (finder map) (bir:inputs iblock))
          (bir:dynamic-environment copy)
          (copy-of (bir:dynamic-environment iblock) map))
    ;; Copy the instructions
    (bir::map-iblock-instructions
     (lambda (inst)
       (let ((cinst (copy-instruction inst stack map))
             (old-pred (bir:predecessor inst)))
         (setf (copy-of inst map) cinst)
         (if (null old-pred)
             (setf (bir:start copy) cinst)
             (let ((new-pred (copy-of old-pred map)))
               (setf (bir:successor new-pred) cinst
                     (bir:predecessor cinst) new-pred)))))
     (bir:start iblock))
    ;; Hook up terminator
    (let ((term (copy-of (bir:end iblock) map)))
      (setf (bir:end copy) term))
    copy))

(defgeneric copy-output (datum stack map))

(defmethod copy-output ((datum bir:output) stack map)
  (declare (ignore stack))
  (setf (copy-of datum map)
        (make-instance 'bir:output
          :name (bir:name datum))))
(defmethod copy-output ((datum bir:phi) stack map)
  (declare (ignore stack))
  (copy-phi datum map))
(defmethod copy-output ((datum bir:variable) stack map)
  (copy-variable datum stack map))

(defun output-copier (stack map)
  (lambda (datum) (copy-output datum stack map)))

;; Usually just needs to find the damn thing from a previous output
(defgeneric copy-input (datum map))
(defmethod copy-input ((datum bir:datum) map)
  (copy-of datum map))
(defmethod copy-input ((datum bir:load-time-value) map)
  (declare (ignore map))
  (make-instance 'bir:load-time-value
    :name (bir:name datum)
    :form (bir:form datum)
    :read-only-p (bir:read-only-p datum)))

(defun input-copier (map)
  (lambda (datum) (copy-input datum map)))

(defgeneric clone-initargs (instruction stack map)
  (:method-combination append))

(defgeneric initialize-copy (copy)
  (:method ((instruction bir:instruction)) instruction))

(defun copy-instruction (instruction stack map)
  (initialize-copy (apply #'make-instance (class-of instruction)
                          (clone-initargs instruction stack map))))

(defmethod initialize-copy :after ((copy bir:terminator))
  (loop with ib = (bir:iblock copy)
        for n in (bir:next copy)
        do (set:nadjoinf (bir:predecessors n) ib)))

(defmethod initialize-copy :after ((term bir:unwind))
  (set:nadjoinf (bir:unwinds
                         (bir:catch term))
                        term)
  (set:nadjoinf (bir:entrances
                         (bir:destination term))
                        (bir:iblock term)))

(defmethod initialize-copy :after ((bind bir:leti))
  (setf (bir:binder bind) (first (bir:outputs bind))))

(defmethod initialize-copy :after ((c bir:catch))
  (set:nadjoinf (bir:catches (bir:function c)) c))

(defmethod initialize-copy :after ((e bir:enclose))
  (setf (bir:enclose (bir:code e)) e))

(defmethod clone-initargs append
    ((instruction bir:instruction) stack map)
  (list
   :inputs (mapcar (input-copier map) (bir:inputs instruction))
   :outputs (mapcar (output-copier stack map)
                    (bir:outputs instruction))
   :iblock (copy-of (bir:iblock instruction) map)
   :policy (bir:policy instruction)
   :origin (bir:origin instruction)))

(defmethod clone-initargs append
    ((instruction bir:terminator) stack map)
  (declare (ignore stack))
  (list
   :next (mapcar (finder map) (bir:next instruction))))

(defmethod clone-initargs append
    ((instruction bir:enclose) stack map)
  (list
   :code (%copy-function (bir:code instruction) stack map)))

(defmethod clone-initargs append
    ((instruction bir:primop) stack map)
  (declare (ignore stack map))
  (list
   :info (bir:info instruction)))

(defmethod clone-initargs append
    ((instruction bir:abstract-call) stack map)
  (declare (ignore stack map))
  (list
   :attributes (bir:attributes instruction)))

(defmethod clone-initargs append
    ((instruction bir:unwind) stack map)
  (let* ((original-dest (bir:destination instruction))
         (original-dest-function (bir:function original-dest))
         (original-catch (bir:catch instruction)))
    (if (member original-dest-function stack)
        (list :catch (copy-of original-catch map)
              :destination (copy-of original-dest map))
        (list :catch original-catch :destination original-dest))))

(defmethod clone-initargs append
    ((instruction bir:jump) stack map)
  (declare (ignore stack map)))

(defmethod clone-initargs append
    ((instruction bir:typeq-test) stack map)
  (declare (ignore stack map))
  (list :ctype (bir:test-ctype instruction)))

(defmethod clone-initargs append
    ((instruction bir:case) stack map)
  (declare (ignore stack map))
  (list :comparees (bir:comparees instruction)))
