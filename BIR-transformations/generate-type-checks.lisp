(in-package #:cleavir-bir-transformations)

;;;; This is a BIR post-pass which takes THEI and generates the type
;;;; check by inlining or local calling the type check function stored
;;;; in the instruction. If there is a manifest type conflict, we warn
;;;; about it here.

;;;; This pass should run before function environments are determined,
;;;; since it may inline functions.

;;; Warn about a compile time type conflict.
(defun maybe-warn-type-conflict (client thei)
  (let ((input (bir:input thei)))
    (when (ctype:values-disjointp client
                                  (bir:asserted-type thei)
                                  (bir:ctype input))
      (warn 'bir:type-conflict
            :datum input
            :asserted-type (bir:asserted-type thei)
            :derived-type (bir:ctype input)
            :asserted-by thei))))

;;; replace a thei with an mv local call.
;;; note that we actually make a new call and delete the thei instruction.
;;; this is a little easier on our consistency checks than change-class is.
(defun thei->mv-call (thei)
  ;; See AST-to-BIR/compile-multiple-value-related-asts.lisp
  ;; We split the iblock twice, in order to transform
  ;; a single block {... -> thei -> ...}
  ;; into three blocks
  ;; {... -> values-collect} -> {mv-local-call -> jump} -> {...}
  (let* ((type-check-function (bir:type-check-function thei))
         (input (bir:input thei))
         (output (bir:output thei))
         (origin (bir:origin thei)) (policy (bir:policy thei))
         (before (bir:split-block-after thei))
         (mv-block (nth-value 1 (bir:split-block-after thei)))
         (collect-out (make-instance 'bir:output
                        :derived-type (bir:ctype output)))
         (collect (make-instance 'bir:values-collect
                    :inputs (list input)
                    :outputs (list collect-out)
                    :origin origin :policy policy
                    :next (list mv-block)))
         (call (make-instance 'bir:mv-local-call
                 :inputs (list type-check-function collect-out)
                 :outputs (list output)
                 :origin origin :policy policy)))
    (bir:replace-terminator collect (bir:end before))
    (setf (bir:dynamic-environment mv-block) collect)
    (bir:insert-instruction-before call (bir:end mv-block))))

(defun generate-type-check (client thei)
  (let ((type-check-function (bir:type-check-function thei)))
    (unless (symbolp type-check-function)
      ;; If the input to the type check is known to be single valued,
      ;; just do a normal call. Otherwise do the much more complex mv call.
      (let* ((input (bir:input thei))
             (ctype (bir:ctype input)))
        (if (and (= (length (ctype:values-required client ctype)) 1)
                 (null (ctype:values-optional client ctype))
                 (ctype:bottom-p client (ctype:values-rest client ctype)))
            (let ((call (make-instance 'bir:local-call
                          :inputs (list type-check-function input)
                          :outputs (list (bir:output thei))
                          :origin (bir:origin thei)
                          :policy (bir:policy thei))))
              (bir:insert-instruction-before call thei))
            (thei->mv-call thei))
        ;; Delete the THEI; make sure the type-check-function is no longer
        ;; relevant
        (setf (bir:type-check-function thei) nil)
        (bir:delete-instruction thei)))))

(defun generate-type-checks (client function)
  (let ((theis '()))
    (bir:do-iblocks (iblock function)
      (bir:do-iblock-instructions (instruction iblock)
        (when (typep instruction 'bir:thei)
          (push instruction theis))))
    ;; We first warn about type conflicts in case we lose derived
    ;; types when generating type checks.
    (dolist (thei theis) (maybe-warn-type-conflict client thei))
    (mapc (lambda (thei) (generate-type-check client thei)) theis)))

(defun module-generate-type-checks (client module)
  (bir:do-functions (function module)
    (generate-type-checks client function)))
