(in-package #:cleavir-bir-builder)

(defclass inserter ()
  ((%iblock :reader bir:iblock :accessor iblock)
   (%insert-point :accessor insert-point
                  ;; null means no instructions have been added yet.
                  :type (or null bir:instruction))))

;;; Some convenience methods.
(defmethod bir:function ((inserter inserter)) (bir:function (iblock inserter)))
(defmethod bir:dynamic-environment ((inserter inserter))
  (bir:dynamic-environment (iblock inserter)))
(defmethod bir:module ((inserter inserter)) (bir:module (bir:function inserter)))

(defun constant (inserter value)
  (bir:constant-in-module value (bir:module inserter)))
(defun vcell (inserter symbol)
  (bir:variable-cell-in-module symbol (bir:module inserter)))
(defun fcell (inserter fname)
  (bir:function-cell-in-module fname (bir:module inserter)))

;; Put the insert point at the last instruction in the block.
;; (The terminator shouldn't be set yet.)
(defun proceed (inserter iblock)
  (setf (iblock inserter) iblock
        (insert-point inserter)
        (if (bir:startedp iblock)
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

(defun make-iblock (inserter
                    &key (function (bir:function inserter))
                      (dynamic-environment
                       (bir:dynamic-environment (iblock inserter)))
                      (ninputs 0)
                      name)
  (let* ((ib (make-instance 'bir:iblock
               :name name
               :function function :dynamic-environment dynamic-environment))
         (phis (loop repeat ninputs
                     collect (make-instance 'bir:phi :iblock ib))))
    (setf (bir:inputs ib) phis)
    (set:nadjoinf (bir:scope dynamic-environment) ib)
    ib))

;;; Function called to carry out any convenient post-processing
;;; called when an instruction is built.
;;; NOTE: An alternate design would be to make INSERT and TERMINATE generic,
;;; and then have :after methods, but this leads to stereotypically ignored
;;; initargs and some ugliness with the compiler macro.
;;; NOTE: It might be even better to have these as just INITIALIZE-INSTANCE
;;; methods for the instructions.
(defgeneric post (instruction)
  ;; Usually, nothing needs to be done.
  (:method ((inst bir:instruction))))

;;; insert an instruction at the end of the iblock.
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
  (post instruction)
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
  (post terminator)
  terminator)

(defun terminate (inserter datum &rest initargs)
  (%terminate inserter (apply #'instruction datum initargs)))
(define-compiler-macro terminate (inserter datum &rest initargs)
  `(%terminate ,inserter (instruction ,datum ,@initargs)))

(defmethod post ((inst bir:leti))
  (let ((var (bir:output inst)))
    (set:nadjoinf (bir:variables (bir:function inst)) var)
    (setf (bir:binder var) inst)))

(defmethod post ((inst bir:readvar))
  (bir:record-variable-ref (bir:input inst)))

(defmethod post ((inst bir:writevar))
  (bir:record-variable-set (bir:output inst)))

(defmethod post ((inst bir:enclose))
  (setf (bir:enclose (bir:code inst)) inst))

(defmethod post ((inst bir:returni))
  (setf (bir:returni (bir:function inst)) inst))

(defmethod post ((inst bir:values-save))
  (setf (bir:dynamic-environment (first (bir:next inst))) inst))
(defmethod post ((inst bir:values-collect))
  (setf (bir:dynamic-environment (first (bir:next inst))) inst))

(defmethod post ((inst bir:come-from))
  (set:nadjoinf (bir:come-froms (bir:function inst)) inst)
  ;; Not any others, since they may be a block merge afterwards.
  (setf (bir:dynamic-environment (first (bir:next inst))) inst))

(defmethod post ((inst bir:unwind))
  (set:nadjoinf (bir:unwinds (bir:come-from inst)) inst)
  (set:nadjoinf (bir:entrances (bir:destination inst)) (bir:iblock inst)))

(defmethod post ((inst bir:constant-bind))
  (setf (bir:dynamic-environment (first (bir:next inst))) inst))
