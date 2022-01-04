(in-package #:cleavir-bir-transformations)

;;;; This is a BIR post-pass which takes THEI and generates the type
;;;; check by inlining or local calling the type check function stored
;;;; in the instruction. If there is a manifest type conflict, we warn
;;;; about it here.

;;;; This pass should run before function environments are determined,
;;;; since it may inline functions.

;;; Warn about a compile time type conflict.
(defun maybe-warn-type-conflict (thei system)
  (let ((input (bir:input thei)))
    (when (ctype:values-disjointp (bir:asserted-type thei) (bir:ctype input)
                                  system)
      (warn 'bir:type-conflict
            :datum input
            :asserted-type (bir:asserted-type thei)
            :derived-type (bir:ctype input)
            :asserted-by thei
            :origin (bir:origin thei)))))

(defun generate-type-check (thei)
  (let ((type-check-function (bir:type-check-function thei)))
    (unless (symbolp type-check-function)
      ;; See AST-to-BIR/compile-multiple-value-related-asts.lisp
      ;; We split the iblock twice, in order to transform
      ;; a single block {... -> thei -> ...}
      ;; into three blocks
      ;; {... -> values-collect} -> {mv-local-call -> jump} -> {...}
      (let* ((input (bir:input thei))
             (output (bir:output thei))
             (origin (bir:origin thei)) (policy (bir:policy thei))
             (before (bir:split-block-after thei))
             (mv-block (nth-value 1 (bir:split-block-after thei)))
             (function (bir:function thei))
             (collect-out (make-instance 'bir:output
                            :derived-type (bir:ctype output)))
             (collect (make-instance 'bir:values-collect
                        :inputs (list input)
                        :outputs (list collect-out)
                        :origin origin :policy policy
                        :next (list mv-block)))
             (call (make-instance 'bir:mv-local-call
                     :inputs (list type-check-function collect-out)
                     :outputs (list (bir:output thei))
                     :origin origin :policy policy)))
        (bir:replace-terminator collect (bir:end before))
        (setf (bir:dynamic-environment mv-block) collect)
        (bir:insert-instruction-before call (bir:end mv-block))
        ;; Delete the THEI; make sure the type-check-function is no longer
        ;; relevant
        (setf (bir:type-check-function thei) :external)
        (bir:delete-instruction thei)
        (bir:verify (bir:module function))))))

(defun generate-type-checks (function system)
  (let ((theis '()))
    (bir:do-iblocks (iblock function)
      (bir:do-iblock-instructions (instruction iblock)
        (when (typep instruction 'bir:thei)
          (push instruction theis))))
    ;; We first warn about type conflicts in case we lose derived
    ;; types when generating type checks.
    (dolist (thei theis) (maybe-warn-type-conflict thei system))
    (mapc #'generate-type-check theis)))

(defun module-generate-type-checks (module system)
  (bir:do-functions (function module)
    (generate-type-checks function system)))
