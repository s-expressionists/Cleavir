(in-package #:cleavir-bir-transformations)

;;; We just attempted to detect local calls. See if anything is worth
;;; doing after. FIXME: Think of a nice CLOSy way to make this optional
;;; and specializable.
(defun post-find-local-calls (function)
  (maybe-interpolate function))

;;; Return true if the call arguments are compatible with those of the
;;; function lambda list. If they're not, warn and return false.
;;; This checks argument counts but does NOT check &key argument validity,
;;; which for non-constant keywords can involve complex analysis.
(defun check-argument-list-compatible (arguments function)
  (let ((lambda-list (cleavir-bir:lambda-list function))
        (nsupplied (length arguments))
        (nrequired 0)
        (noptional 0)
        (restp nil))
    (cleavir-bir:map-lambda-list
     (lambda (state item index)
       (declare (ignore item index))
       (case state
         (:required (incf nrequired))
         (&optional (incf noptional))
         ((&rest &key) (setf restp t))
         (&allow-other-keys)
         (otherwise
          ;; Implementation-specific keyword. We don't know how to deal
          ;; with this, so silently give up. KLUDGEy.
          (return-from check-argument-list-compatible nil))))
     lambda-list)
    (let ((nfixed (+ nrequired noptional)))
      (if (and (<= nrequired nsupplied) (or restp (<= nsupplied nfixed)))
          t
          (warn "~a is passed ~d arguments, but expects ~@?"
                (cleavir-bir:name function) nsupplied
                (cond (restp "at least ~d")
                      ((zerop noptional) "exactly ~d")
                      ((zerop nrequired) "at most ~*~d")
                      (t "between ~d and ~d"))
                nrequired nfixed)))))

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
  (let ((enclose (cleavir-bir:enclose function)))
    (when enclose
      (let* ((eout (cleavir-bir:output enclose))
             (use (cleavir-bir:use eout)))
        (typecase use
          (cleavir-bir:call
           (when (and (eq eout (cleavir-bir:callee use))
                      (check-argument-list-compatible
                       (rest (cleavir-bir:inputs use))
                       function))
             (change-class use 'cleavir-bir:local-call)
             (cleavir-bir:replace-uses function eout)
             (cleavir-bir:delete-instruction enclose)))
          (cleavir-bir:mv-call
           (when (eq eout (cleavir-bir:callee use))
             (change-class use 'cleavir-bir:mv-local-call)
             (cleavir-bir:replace-uses function eout)
             (cleavir-bir:delete-instruction enclose)))
          (cleavir-bir:leti
           (let ((variable (cleavir-bir:output use)))
             ;; Variable needs to be immutable since we want to make
             ;; sure this definition reaches the readers.
             (when (cleavir-bir:immutablep variable)
               (cleavir-set:doset (reader (cleavir-bir:readers variable))
                 (let* ((rout (cleavir-bir:output reader))
                        (use (cleavir-bir:use rout)))
                   (typecase use
                     (cleavir-bir:call
                      (when (and (eq rout (cleavir-bir:callee use))
                                 (check-argument-list-compatible
                                  (rest (cleavir-bir:inputs use))
                                  function))
                        (change-class use 'cleavir-bir:local-call)
                        (cleavir-bir:replace-uses function rout)
                        (cleavir-bir:delete-instruction reader)))
                     (cleavir-bir:mv-call
                      (when (eq rout (cleavir-bir:callee use))
                        (change-class use 'cleavir-bir:mv-local-call)
                        (cleavir-bir:replace-uses function rout)
                        (cleavir-bir:delete-instruction reader)))))))
             ;; No more references to the variable means we can clean
             ;; up the enclose. The LETI might've already been
             ;; cleaned up by any readvar deletion triggers.
             (when (cleavir-set:empty-set-p (cleavir-bir:readers variable))
               (unless (cleavir-set:empty-set-p (cleavir-bir:writers variable))
                 (cleavir-bir:delete-instruction use))
               (cleavir-bir:delete-instruction enclose))))))))
  (post-find-local-calls function))

(defun find-module-local-calls (module)
  (cleavir-bir:map-functions #'find-function-local-calls module)
  ;; Since contification depends on all non-tail local calls being in
  ;; the same function, it may be the case that contifying triggers
  ;; more contification. Therefore, we do a second pass/fixpoint loop
  ;; to make sure everything gets contified. This also ensures that we
  ;; contify deterministically, since the result of a single pass
  ;; depends on the order of iteration over the set of functions in
  ;; the module.
  (let ((did-something nil))
    (loop do (let ((changed nil))
               (cleavir-bir:do-functions (function module)
                 (when (maybe-interpolate function)
                   (setq changed t)))
               (setq did-something changed))
          while did-something)))
