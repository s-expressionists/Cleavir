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
      (fflc-use function (cleavir-bir:output enclose))))
  (post-find-local-calls function))

;;; Find local calls for a given use of a function enclosure.
;;; Because the closure may be stored in a variable or etc, it is convenient
;;; for this function to be recursive.
(defun fflc-use (function datum)
  (let ((use (cleavir-bir:use datum)) (def (cleavir-bir:definition datum)))
    (typecase use
      (cleavir-bir:call
       (when (and (eq datum (cleavir-bir:callee use))
                  (check-argument-list-compatible
                   (rest (cleavir-bir:inputs use))
                   function))
         (change-class use 'cleavir-bir:local-call)
         (cleavir-bir:replace-uses function datum)
         (cleavir-bir:delete-instruction def)))
      (cleavir-bir:mv-call
       (when (eq datum (cleavir-bir:callee use))
         (change-class use 'cleavir-bir:mv-local-call)
         (cleavir-bir:replace-uses function datum)
         (cleavir-bir:delete-instruction def)))
      (cleavir-bir:fixed-to-multiple
       ;; This case is here to catch code like (funcall (values x) ...)
       ;; which can be produced by macros.
       (when (eq datum (first (cleavir-bir:inputs use)))
         (let ((out (cleavir-bir:output use)))
           (fflc-use function out)
           (when (null (cleavir-bir:use out)) ; use was deleted
             (cleavir-bir:delete-instruction def)))))
      (cleavir-bir:leti
       (let ((variable (cleavir-bir:output use)))
         ;; Variable needs to be immutable since we want to make
         ;; sure this definition reaches the readers.
         (when (cleavir-bir:immutablep variable)
           (cleavir-set:doset (reader (cleavir-bir:readers variable))
             (fflc-use function (cleavir-bir:output reader)))
           ;; No more references to the variable means we can clean
           ;; up the enclose.
           (when (cleavir-set:empty-set-p (cleavir-bir:readers variable))
             ;; The LETI might've already been
             ;; cleaned up by any readvar deletion triggers.
             (unless (cleavir-set:empty-set-p (cleavir-bir:writers variable))
               (cleavir-bir:delete-instruction use))
             (cleavir-bir:delete-instruction def)))))
      (null ; unused
       (cleavir-bir:delete-instruction def)))))

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
