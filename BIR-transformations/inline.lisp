(in-package #:cleavir-bir-transformations)

;; return a list of call instructions for this enclose that could be inlined.
;; extremely simplistic at the moment; also it actually only looks for
;; interpolatability specifically (so e.g. there must be only one call).
(defun potential-inlines (enclose)
  (check-type enclose cleavir-bir:enclose)
  (typecase (cleavir-bir:use enclose)
    (cleavir-bir:call
     ;; If it's only used for a call, it must be inlinable, as it must be in
     ;; the same function as the enclose i.e. not recursive.
     (list (cleavir-bir:use enclose)))
    (t nil)))

;; required parameters only. rip.
(defun lambda-list-inlinable-p (lambda-list)
  (every (lambda (a) (typep a 'cleavir-bir:argument)) lambda-list))

(defun inline-functions (ir)
  (cleavir-bir:map-instructions
   (lambda (i)
     (when (typep i 'cleavir-bir:enclose)
       (let ((inlines (potential-inlines i))
             (code (cleavir-bir:code i)))
         (when (and (lambda-list-inlinable-p (cleavir-bir:lambda-list code))
                    (= (length inlines) 1))
           (interpolate-function code (first inlines))))))
   ir))
