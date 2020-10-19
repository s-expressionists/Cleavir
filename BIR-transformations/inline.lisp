(in-package #:cleavir-bir-transformations)

;; We just attempted to detect local calls. See if anything is worth
;; doing after. FIXME: Think of a nice CLOSy way to make this optional
;; and specializable.
(defun post-find-local-calls (function)
  ;; When a function has no encloses and has only one local call, it
  ;; is eligible for interpolation.
  (when (cleavir-set:empty-set-p (cleavir-bir:encloses function))
    ;; FIXME: We could contify more generally here.
    (let ((local-calls (cleavir-bir:local-calls function)))
      (when (= (cleavir-set:size local-calls) 1)
        (let ((local-call (cleavir-set:arb local-calls)))
          (when (lambda-list-inlinable-p (cleavir-bir:lambda-list function))
            (interpolate-function function local-call)))))))

;; required parameters only. rip.
(defun lambda-list-inlinable-p (lambda-list)
  (every (lambda (a) (typep a 'cleavir-bir:argument)) lambda-list))

;; Return true if the call arguments are compatible with those of the function.
;; If they're not, warn and return false.
(defun check-argument-list-compatible (arguments function)
  (let ((lambda-list (cleavir-bir:lambda-list function)))
    (let ((nsupplied (length arguments))
          (nrequired (length lambda-list)))
      (if (= nsupplied nrequired)
          t
          (warn "Expected ~a required arguments but got ~a arguments for function ~a."
                nrequired nsupplied (cleavir-bir:name function))))))

;; Detect calls to a function via its closure and mark them as direct
;; local calls to the function. If there are no more references to the
;; closure, we can clean it up. This allows us to avoid allocating
;; closures for functions which only have local calls. Check that the
;; call and the lambda list have compatible arguments, flaming and not
;; converting if it doesn't so we can get a runtime error. This serves
;; as a sort of "escape analysis" for functions, recording the result
;; of the analysis directly into the IR.
(defun find-function-local-calls (function)
  ;; FIXME: Arg parsing code not yet written!
  (when (lambda-list-inlinable-p (cleavir-bir:lambda-list function))
    (cleavir-set:doset (enclose (cleavir-bir:encloses function))
      (when (cleavir-bir:unused-p enclose)
        ;; FIXME: Note this dead code.
        (cleavir-bir:delete-computation enclose)
        (cleavir-bir:remove-function-from-module function)
        (return-from find-function-local-calls))
      (let ((use (cleavir-bir:use enclose)))
        (typecase use
          (cleavir-bir:call
           (when (eq enclose (cleavir-bir:callee use))
             (when (check-argument-list-compatible (rest (cleavir-bir:inputs use))
                                                   function)
               (change-class use 'cleavir-bir:local-call)
               (cleavir-bir:replace-computation enclose function)))
           (when (cleavir-bir:unused-p enclose)
             (cleavir-bir:delete-computation enclose)))
          (cleavir-bir:writevar
           (let ((variable (first (cleavir-bir:outputs use))))
             ;; Variable needs to be immutable since we want to make
             ;; sure this definition reaches the readers.
             (when (cleavir-bir:immutablep variable)
               (cleavir-set:doset (reader (cleavir-bir:readers variable))
                 (unless (cleavir-bir:unused-p reader)
                   (let ((use (cleavir-bir:use reader)))
                     (typecase use
                       (cleavir-bir:call
                        ;; Check the function is in call position for this call.
                        (when (or (eq use (cleavir-bir:callee use))
                                  (not (member reader
                                               (rest
                                                (cleavir-bir:inputs use)))))
                          (when (check-argument-list-compatible
                                 (rest (cleavir-bir:inputs use))
                                 function)
                            (change-class use 'cleavir-bir:local-call)
                            (cleavir-bir:replace-computation reader function)))))))))
             ;; No more references to the variable means we can clean up
             ;; the writer and enclose.
             (when (cleavir-set:empty-set-p (cleavir-bir:readers variable))
               (cleavir-bir:delete-instruction use)
               (cleavir-bir:delete-computation enclose)))))))
    (post-find-local-calls function)))

(defun find-module-local-calls (module)
  (cleavir-set:mapset nil #'find-function-local-calls
                      (cleavir-bir:functions module)))
