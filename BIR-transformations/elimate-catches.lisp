(in-package #:cleavir-bir-transformations)

(defun catch-eliminable-p (catch)
  (cleavir-set:empty-set-p (cleavir-bir:unwinds catch)))

(defun eliminate-catch (catch)
  (let ((nde (cleavir-bir:dynamic-environment catch)))
    (cleavir-set:doset (s (cleavir-bir:scope catch))
      (setf (cleavir-bir:dynamic-environment s) nde))
    ;; Replace the instruction
    (cleavir-bir:replace-terminator
     (make-instance 'cleavir-bir:jump
       :inputs () :outputs () :unwindp nil
       :next (list (first (cleavir-bir:next catch))))
     catch)))

(defun eliminate-catches (function)
  (cleavir-bir:map-iblocks
   (lambda (ib)
     (let ((end (cleavir-bir:end ib)))
       (when (and (typep end 'cleavir-bir:catch)
                  (catch-eliminable-p end))
         (eliminate-catch end))))
   function))

(defun module-eliminate-catches (module)
  (cleavir-set:mapset nil #'eliminate-catches
                      (cleavir-bir:functions module)))
