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
  (let ((input (bir:input thei))
        (type-check-function (bir:type-check-function thei)))
    (unless (symbolp type-check-function)
      (change-class thei 'bir:mv-local-call
                    :inputs (list type-check-function input)))))

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
