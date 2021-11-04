(in-package #:cleavir-bir-transformations)

;;; We just attempted to detect local calls. See if anything is worth
;;; doing after. FIXME: Think of a nice CLOSy way to make this optional
;;; and specializable.
(defun post-find-local-calls (function system)
  (maybe-interpolate function system))

;;; Return true if the call arguments are compatible with those of the
;;; function lambda list. If they're not, warn and return false.
;;; This checks argument counts but does NOT check &key argument validity,
;;; which for non-constant keywords can involve complex analysis.
(defun check-argument-list-compatible (arguments function system)
  (let* ((lambda-list (bir:lambda-list function))
         (nsupplied (length arguments))
         (nrequired
           (length (lambda-list:required-parameters lambda-list system)))
         (noptional
           (length (lambda-list:optional-parameters lambda-list system)))
         (restp (or (lambda-list:rest-parameter lambda-list system)
                    (lambda-list:keys-p lambda-list system))))
    (when (notevery
           (lambda (pg)
             (lambda-list:standard-parameter-group-p pg system))
           (lambda-list:parameter-groups lambda-list system))
      ;; Implementation-specific keyword. We don't know how to deal
      ;; with this, so silently give up. KLUDGEy.
      (return-from check-argument-list-compatible nil))
    (let ((nfixed (+ nrequired noptional)))
      (if (and (<= nrequired nsupplied) (or restp (<= nsupplied nfixed)))
          t
          (warn "~a is passed ~d arguments, but expects ~@?"
                (bir:name function) nsupplied
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
(defun find-function-local-calls (function system)
  (let ((enclose (bir:enclose function)))
    (when enclose
      (fflc-use function (bir:output enclose) system)))
  (post-find-local-calls function system))

;;; Find local calls for a given use of a function enclosure.
;;; Because the closure may be stored in a variable or etc, it is convenient
;;; for this function to be recursive.
(defun fflc-use (function datum system)
  (let ((use (bir:use datum)) (def (bir:definition datum)))
    (typecase use
      (bir:call
       (when (and (eq datum (bir:callee use))
                  (check-argument-list-compatible
                   (rest (bir:inputs use))
                   function system))
         (change-class use 'bir:local-call)
         (bir:replace-uses function datum)
         (bir:delete-instruction def)))
      (bir:mv-call
       (when (eq datum (bir:callee use))
         (change-class use 'bir:mv-local-call)
         (bir:replace-uses function datum)
         (bir:delete-instruction def)))
      (bir:fixed-to-multiple
       ;; This case is here to catch code like (funcall (values x) ...)
       ;; which can be produced by macros.
       (when (eq datum (first (bir:inputs use)))
         (let ((out (bir:output use)))
           (fflc-use function out system)
           (when (null (bir:use out)) ; use was deleted
             (bir:delete-instruction def)))))
      (bir:leti
       (let ((variable (bir:output use)))
         ;; Variable needs to be immutable since we want to make
         ;; sure this definition reaches the readers.
         (when (bir:immutablep variable)
           (set:doset (reader (bir:readers variable))
             (fflc-use function (bir:output reader) system))
           ;; No more references to the variable means we can clean
           ;; up the enclose.
           (when (set:empty-set-p (bir:readers variable))
             ;; The LETI might've already been
             ;; cleaned up by any readvar deletion triggers.
             (unless (set:empty-set-p (bir:writers variable))
               (bir:delete-instruction use))
             (bir:delete-instruction def)))))
      (null ; unused
       (bir:delete-instruction def)))))

(defun find-module-local-calls (module system)
  (bir:do-functions (function module)
    (find-function-local-calls function system))
  ;; Since contification depends on all non-tail local calls being in
  ;; the same function, it may be the case that contifying triggers
  ;; more contification. Therefore, we do a second pass/fixpoint loop
  ;; to make sure everything gets contified. This also ensures that we
  ;; contify deterministically, since the result of a single pass
  ;; depends on the order of iteration over the set of functions in
  ;; the module.
  (let ((did-something nil))
    (loop do (let ((changed nil))
               (bir:do-functions (function module)
                 (when (maybe-interpolate function system)
                   (setq changed t)))
               (setq did-something changed))
          while did-something)))
