(in-package #:cleavir-bir-transformations)

(defun lambda-list-too-hairy-p (lambda-list)
  (not (every (lambda (item)
                (or (not (symbolp item))
                    (eq item '&optional)))
              lambda-list)))

;;; This utility parses BIR lambda lists. FUNCTION takes two
;;; arguments: the current lambda-list item being parsed and the state
;;; of the parse (i.e. &OPTIONAL). Does not yet deal with &REST or
;;; &KEY.
(defun map-lambda-list (function lambda-list)
  (assert (not (lambda-list-too-hairy-p lambda-list)))
  (let ((state :required))
    (dolist (item lambda-list)
      (if (eq item '&optional)
          (setq state :optional)
          (funcall function state item)))))

;;; We just attempted to detect local calls. See if anything is worth
;;; doing after. FIXME: Think of a nice CLOSy way to make this optional
;;; and specializable.
(defun post-find-local-calls (function)
  (maybe-interpolate function))

;;; Return true if the call arguments are compatible with those of the
;;; function lambda list. If they're not, warn and return false.
(defun check-argument-list-compatible (arguments function)
  (let ((lambda-list (cleavir-bir:lambda-list function))
        (nsupplied (length arguments))
        (nrequired 0)
        (noptional 0))
    (assert (not (lambda-list-too-hairy-p lambda-list)))
    (map-lambda-list (lambda (state item)
                       (ecase state
                         (:required (incf nrequired))
                         (:optional (incf noptional))))
                     lambda-list)
    (if (<= nrequired nsupplied (+ noptional nrequired))
        t
        (warn "Expected ~a required arguments and ~a optional arguments but got ~a arguments for function ~a."
              nrequired noptional nsupplied (cleavir-bir:name function)))))

;;; Detect calls to a function via its closure and mark them as direct
;;; local calls to the function, doing compile time argument
;;; checking. If there are no more references to the closure, we can
;;; clean it up. This allows us to avoid allocating closures for
;;; functions which only have local calls. If the call arguments and
;;; the lambda list are not compatible, flame and do not convert so we
;;; can get a runtime error. This also serves as a sort of "escape
;;; analysis" for functions, recording the result of the analysis
;;; directly into the IR.
(defun find-function-local-calls (function)
  ;; FIXME: Arg parsing code not yet written!
  (unless (lambda-list-too-hairy-p (cleavir-bir:lambda-list function))
    (cleavir-set:doset (enclose (cleavir-bir:encloses function))
      (when (cleavir-bir:unused-p enclose)
        ;; FIXME: Note this dead code.
        (cleavir-bir:delete-computation enclose)
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
                        (when (eq reader (cleavir-bir:callee use))
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
                      (cleavir-bir:functions module))
  ;; Since contification depends on all non-tail local calls being in
  ;; the same function, it may be the case that contifying triggers
  ;; more contification. Therefore, we do a second pass/fixpoint loop
  ;; to make sure everything gets contified. This also ensures that we
  ;; contify deterministically, since the result of a single pass
  ;; depends on the order of iteration over the set of functions in
  ;; the module.
  (let ((did-something nil))
    (loop do (let ((changed nil))
               (cleavir-set:doset (function (cleavir-bir:functions module))
                 (when (maybe-interpolate function)
                   (setq changed t)))
               (setq did-something changed))
          while did-something)))
