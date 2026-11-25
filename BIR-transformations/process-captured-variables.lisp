(in-package #:cleavir-bir-transformations)

;;; Add LEXICAL onto the environment of ACCESS-FUNCTION and do so
;;; recursively.
(defun close-over (function access-function lexical)
  (unless (eq function access-function)
    (let ((environment (bir:environment access-function)))
      (unless (set:presentp lexical environment)
        (set:nadjoinf environment lexical)
        (let ((enclose (bir:enclose access-function)))
          (when enclose
            (close-over function (bir:function enclose) lexical)))
        (set:doset (local-call (bir:local-calls access-function))
          (close-over function (bir:function local-call) lexical))))))

;;; Fill in the environments of every function.
(defun determine-function-environments (module)
  (bir:do-functions (function module)
    (set:doset (variable (bir:variables function))
      (set:doset (reader (bir:readers variable))
        (close-over function (bir:function reader) variable))
      (set:doset (writer (bir:writers variable))
        (close-over function (bir:function writer) variable)))
    (set:doset (come-from (bir:come-froms function))
      (set:doset (unwind (bir:unwinds come-from))
        (close-over function (bir:function unwind) come-from)))))

;;; Determine the extent of closures. We mark closures created by
;;; ENCLOSE instructions as dynamic extent if all of its uses are
;;; calls with the DX-call attribute in the enclosing function.
;;; We only mark closures as dynamic extent and do not try to mark a
;;; function as indefinite extent, since there may be an explicit
;;; dynamic extent declaration on the function which we should preserve.

;;; This analysis is conservative and could be improved (FIXME).
;;; Functions that are closed over are never marked dynamic-extent, even
;;; in cases in which they could be, e.g. if they are only recursive.

;;; A CL:DYNAMIC-EXTENT declaration on the binding additionally lets us walk
;;; into local callees, which is how a closure handed to an FLET or LABELS
;;; function gets stack allocated. The declaration is only permission to try:
;;; the escape analysis below still has to succeed on its own, so a declaration
;;; that is in fact wrong costs the optimisation and not memory safety.
(defun safe-call-p (call)
  (attributes:has-flag-p (bir:attributes call) :dx-call))

(defun declared-dynamic-extent-p (variable)
  (bir:dynamic-extent (bir:binder variable)))

;;; The module function a callee datum designates, or NIL. A front end that
;;; builds BIR from an already compiled representation can leave a call to a
;;; local function as a plain CALL whose callee is still statically an ENCLOSE
;;; of a function in this module, so FIND-LOCAL-CALLS is not the only way for
;;; the callee to be known.
(defun resolve-callee-function (datum &optional (depth 0))
  (when (and datum (< depth 4))
    (let ((definition (bir:definition datum)))
      (typecase definition
        (bir:enclose (bir:code definition))
        (bir:readvar
         (let ((variable (bir:input definition)))
           (when (bir:immutablep variable)
             (resolve-callee-function (bir:input (bir:binder variable))
                                      (1+ depth)))))
        (t nil)))))

;;; The CALLEE parameter DATUM is passed as. NIL unless it is a required
;;; parameter passed exactly once, with nothing but required parameters ahead
;;; of it, which keeps the position-to-parameter mapping unambiguous.
(defun call-required-parameter (call callee datum)
  (let ((arguments (rest (bir:inputs call))))
    (when (= 1 (count datum arguments))
      (let ((pos (position datum arguments))
            (lambda-list (bir:lambda-list callee)))
        (when (and (< pos (length lambda-list))
                   (loop for item in lambda-list
                         repeat (1+ pos)
                         always (typep item 'bir:argument)))
          (nth pos lambda-list))))))

;;; True if this USE of DATUM cannot let the value out of OWNER. A use has to
;;; live in OWNER itself, since one inside a nested closure could run later.
;;; A parameter is not always bound to a variable -- a front end need not emit a
;;; LETI for one that is read once -- so both shapes have to be handled here.
(defun use-retains-nothing-p (use datum owner follow-calls visited)
  (typecase use
    (null t)
    ((or bir:call bir:local-call)
     (and (eq (bir:function use) owner)
          (call-retains-nothing-p use datum follow-calls visited)))
    (bir:leti
     (not (value-escapes-p (bir:output use) owner follow-calls visited)))
    (t nil)))

;;; True if VARIABLE's value can still be reached once OWNER's frame is gone.
(defun value-escapes-p (variable owner follow-calls visited)
  (cond ((set:presentp variable visited) nil)
        (t
         (set:nadjoinf visited variable)
         (set:doset (reader (bir:readers variable))
           (let ((rout (bir:output reader)))
             (unless (use-retains-nothing-p (bir:use rout) rout owner
                                            follow-calls visited)
               (return-from value-escapes-p t))))
         nil)))

(defun call-retains-nothing-p (call datum follow-calls visited)
  (cond
    ;; Occupying the callee position is not an escape: invoking a function does
    ;; not retain it.
    ((eq datum (bir:callee call)) t)
    ((safe-call-p call) t)
    ;; Otherwise we can walk into a statically known callee, but only with the
    ;; user's permission, since that walk is interprocedural.
    (follow-calls
     (let ((callee (if (typep call 'bir:abstract-local-call)
                       (bir:callee call)
                       (resolve-callee-function (bir:callee call)))))
       (and (typep callee 'bir:function)
            (not (callee-lets-escape-p call callee datum visited)))))
    (t nil)))

;;; True if handing DATUM to CALL can let it outlive that call.
(defun callee-lets-escape-p (call callee datum visited)
  (let ((parameter (call-required-parameter call callee datum)))
    (cond ((null parameter) t)
          ;; A cycle through mutually recursive calls contributes no use that
          ;; has not already been checked on the way in.
          ((set:presentp parameter visited) nil)
          (t
           (set:nadjoinf visited parameter)
           (not (use-retains-nothing-p (bir:use parameter) parameter
                                       callee t visited))))))

(defun determine-closure-extent (function)
  (let ((enclose (bir:enclose function)))
    (when enclose
      (let* ((eout (bir:output enclose))
             (use (bir:use eout)))
        (typecase use
          (bir:call (when (safe-call-p use)
                      (setf (bir:extent enclose) :dynamic)))
          (bir:writevar
           (let ((variable (bir:output use)))
             ;; Only after every reader has been checked.
             (unless (value-escapes-p variable (bir:function enclose)
                                      (declared-dynamic-extent-p variable)
                                      (set:empty-set))
               (setf (bir:extent enclose) :dynamic)))))))))

(defun determine-closure-extents (module)
  (bir:map-functions #'determine-closure-extent module))

(defun function-extent (function)
  (let ((enclose (bir:enclose function)))
    (if enclose
        (bir:extent enclose)
        :dynamic)))

;;; Determine the extent of every variable in a function based on the
;;; extent of any functions which close over it.
;;; Precondition: environments of functions must be filled in, and it
;;; helps to also analyze closure extent beforehand.
(defun determine-variable-extents (module)
  ;; First, initialize the extent of every variable.
  (bir:do-functions (function module)
    (set:doset (variable (bir:variables function))
      (setf (bir:extent variable) :local)))
  ;; Fill in the extent of every closed over variable.
  (bir:do-functions (function module)
    (ecase (function-extent function)
      (:dynamic
       (set:doset (lexical (bir:environment function))
         (when (and (typep lexical 'bir:variable)
                    (eq (bir:extent lexical) :local))
           (setf (bir:extent lexical) :dynamic))))
      (:indefinite
       (set:doset (lexical (bir:environment function))
         (when (typep lexical 'bir:variable)
           (setf (bir:extent lexical) :indefinite)))))))
