(in-package #:cleavir-bir-transformations)

;; return a list of call instructions for this enclose that could be inlined.
;; extremely simplistic at the moment; also it actually only looks for
;; interpolatability specifically (so e.g. there must be only one call).
(defun potential-inlines (enclose)
  (check-type enclose cleavir-bir:enclose)
  (unless (cleavir-bir:unused-p enclose)
    (let* ((use (cleavir-bir:use enclose))
           (use-inputs (cleavir-bir:inputs use)))
      (typecase use
        (cleavir-bir:call
         ;; If it's only used for a call, it must be inlinable, as it must be in
         ;; the same function as the enclose i.e. not recursive.
         ;; It does have to be the callee and only the callee, though.
         ;; Since enclose is linear, it only has one use, so we only need to
         ;; check that it's the callee.
         (when (eq enclose (first use-inputs))
           (list (cleavir-bir:use enclose))))
        (t nil)))))

;; required parameters only. rip.
(defun lambda-list-inlinable-p (lambda-list)
  (every (lambda (a) (typep a 'cleavir-bir:argument)) lambda-list))

(defun inline-functions (ir)
  (cleavir-set:doset (function (cleavir-bir:functions (cleavir-bir:module ir)))
    (cleavir-set:doset (enclose (cleavir-bir:encloses function))
      (let ((inlines (potential-inlines enclose)))
        (when (and (lambda-list-inlinable-p (cleavir-bir:lambda-list function))
                   (= (length inlines) 1))
          (interpolate-function function (first inlines))))))
  ;; FIXME: This will no longer be necessary once AST-to-BIR tagbody
  ;; translation is fixed.
  (cleavir-bir:refresh-iblocks (cleavir-bir:module ir)))
