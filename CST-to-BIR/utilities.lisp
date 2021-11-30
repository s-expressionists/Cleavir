(cl:in-package #:cleavir-cst-to-bir)

(defun symbol-macro-expander (expansion)
  (lambda (form env)
    (declare (ignore form env))
    expansion))

(defun expand (expander form env)
  (funcall (coerce *macroexpand-hook* 'cl:function)
           expander form env))

;;; Given the CST for a MACROLET definition and an environment, return
;;; a macro expander (or macro function) for the definition.
;;; FIXME: check syntax.
(defun expander (definition-cst environment system)
  (cst:db origin (name-cst lambda-list-cst . body-cst) definition-cst
    (let ((lambda-expression (cst:parse-macro system
                                              name-cst
                                              lambda-list-cst
                                              (cst:raw body-cst)
                                              environment)))
      (env:eval lambda-expression
                (env:compile-time environment)
                environment))))

;;; This variable is bound to a list of forms that will be
;;; searched for in the CST so that a source position can
;;; be found.
;;; It's a dynamic variable because a macroexpander may
;;; want to bind it; see WITH-CURRENT-SOURCE-FORM.
(defvar *current-source-forms*)

(defun find-source-cst-1 (cst form)
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((aux (cst)
               (unless (gethash cst seen)
                 (setf (gethash cst seen) t)
                 (when (eq (cst:raw cst) form)
                   (return-from find-source-cst-1 cst))
                 ;; We don't search through atoms.
                 (when (typep cst 'cst:cons-cst)
                   (aux (cst:first cst))
                   (aux (cst:rest cst)))
                 nil)))
      (aux cst))))

;;; Used in the encapsulators in conditions.lisp.
(defun find-source-cst (cst
                        &optional (forms *current-source-forms*))
  (if (null forms)
      cst
      (or (find-source-cst-1 cst (first forms))
          (find-source-cst cst (rest forms)))))

;;; This is a helper operator to get more accurate errors from
;;; macroexpansion functions. Cleavir wraps such errors in
;;; other conditions that include the CST, which has source
;;; information, using with-encapsulated-conditions. This
;;; operator can be used to specify which CST should be
;;; included in that encapsulation condition.
;;; *CURRENT-SOURCE-FORMS* is initially bound to NIL. In a
;;; WITH-CURRENT-SOURCE-FORM, it will be rebound to a list
;;; including the given forms. Then, when an error is signaled
;;; and Cleavir encapsulates it, it searches for the form's
;;; corresponding CST using FIND-SOURCE-CST, and reports that CST
;;; with the condition. (The search is delayed until an error is
;;; actually reported so that meaningless work is not done in the
;;; usual case of no errors being reported.)
;;; This is useful for macros like COND and SETF. For example,
;;; (with-current-source-form (place) (get-setf-expansion place))
;;; means that if the place is malformed etc., the error location
;;; is localized to the place, not the entire SETF form.
;;; If WITH-CURRENT-SOURCE-FORM is executed in some context other
;;; than a macroexpander in Cleavir, no special processing is done.
(defmacro with-current-source-form ((&rest forms) &body body)
  ;; This circuitious expansion is to ensure that code using this
  ;; macro can be loaded even if the compiler has not been yet,
  ;; for bootstrapping purposes or otherwise.
  (let ((thunkg (gensym "THUNK")))
    ;; progn to ensure DECLARE doesn't work
    `(flet ((,thunkg () (progn ,@body)))
       (if (boundp '*current-source-forms*)
           (let ((*current-source-forms*
                  (list* ,@forms *current-source-forms*)))
             (,thunkg))
           (,thunkg)))))

(defun expand-macro (expander cst env)
  (with-encapsulated-conditions
      (cst macroexpansion-error
           macroexpansion-warning
           macroexpansion-style-warning)
    (expand expander (cst:raw cst) env)))

(defun expand-compiler-macro (expander cst env)
  (let ((form (cst:raw cst)))
    (restart-case
        (with-encapsulated-conditions
            (cst compiler-macro-expansion-error
                 compiler-macro-expansion-warning
                 compiler-macro-expansion-style-warning)
          (expand expander form env))
      (continue ()
        :report "Ignore compiler macro."
        (return-from expand-compiler-macro form)))))

(defun cst-eval (cst environment system)
  (with-encapsulated-conditions
      (cst eval-error eval-warning eval-style-warning)
    (env:cst-eval cst environment environment system)))

(defun make-atom-cst (object &optional origin)
  (make-instance 'cst:atom-cst :raw object :source origin))

;;; Take a CST, check whether it represents a proper list.  If it does
;;; not represent ERROR-TYPE is a symbol that is passed to ERROR.
(defun check-cst-proper-list (cst error-type &rest more-initargs)
  (unless (cst:proper-list-p cst)
    (apply #'error error-type :cst cst more-initargs)))

;;; Check that the number of arguments greater than or equal to MIN
;;; and less than or equal to MAX.  When MAX is NIL, then there is no
;;; upper bound on the number of arguments.  If the argument count is
;;; wrong, then signal an error.  It is assumed that CST represents a
;;; proper list, so this must be checked first by the caller.
(defun check-argument-count (cst min max)
  (let ((count (1- (length (cst:raw cst)))))
    (unless (and (>= count min)
                 (or (null max)
                     (<= count max)))
      (error 'incorrect-number-of-arguments-error
             :cst cst
             :expected-min min
             :expected-max max
             :observed count))))

;;; Used by both convert-special and toplevel.
(defun check-function-bindings (bindings operator)
  (check-cst-proper-list bindings 'bindings-must-be-proper-list
                         :operator operator)
  (loop for remaining = bindings then (cst:rest remaining)
        until (cst:null remaining)
        do (check-cst-proper-list
            (cst:first remaining)
            'local-function-definition-must-be-proper-list)))

;;; Used by both convert-special and toplevel.
(defun check-eval-when-syntax (cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (let ((situations-cst (cst:second cst)))
    (unless (cst:proper-list-p situations-cst)
      (error 'situations-must-be-proper-list :cst situations-cst))
    ;; Check each situation
    (loop for remaining = situations-cst then (cst:rest remaining)
          until (cst:null remaining)
          do (let* ((situation-cst (cst:first remaining))
                    (situation (cst:raw situation-cst)))
               (unless (and (symbolp situation)
                            (member situation
                                    '(:compile-toplevel :load-toplevel :execute
                                      compile load eval)))
                 (error 'invalid-eval-when-situation :cst situation-cst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FUNCTION.

(defun proper-function-name-p (name-cst)
  (let ((name (cst:raw name-cst)))
    (or (symbolp name)
        (and (cst:proper-list-p name-cst)
             (= (length name) 2)
             (eq (car name) 'setf)
             (symbolp (cadr name))))))
