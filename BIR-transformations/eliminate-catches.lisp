(in-package #:cleavir-bir-transformations)

(defun catch-eliminable-p (catch)
  (cleavir-set:empty-set-p (cleavir-bir:unwinds catch)))

(defun eliminate-catch (catch)
  (let ((nde (cleavir-bir:dynamic-environment catch)))
    (cleavir-set:doset (s (cleavir-bir:scope catch))
                       (setf (cleavir-bir:dynamic-environment s) nde))
    ;; Eliminate the writevar
    (let ((use (cleavir-bir:use catch)))
      (assert (typep use 'cleavir-bir:writevar))
      (cleavir-bir:delete-instruction use))
    ;; Replace the instruction itself
    (cleavir-bir:replace-terminator
     (make-instance 'cleavir-bir:jump
       :inputs () :outputs () :unwindp nil
       :next (list (first (cleavir-bir:next catch))))
     catch)))

(defun eliminate-catches (ir)
  (cleavir-bir:map-instructions
   (lambda (i)
     (when (and (typep i 'cleavir-bir:catch)
                (catch-eliminable-p i))
       (eliminate-catch i)))
   ir))
