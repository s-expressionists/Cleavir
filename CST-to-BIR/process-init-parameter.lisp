(in-package #:cleavir-cst-to-bir)

;;; This function takes care of generating code to initialize optional/key
;;; parameters from initforms. VAR-CST, SUPPLIED-P-CST, and INITFORM-CST
;;; are the relevant CSTs. VAR-VAL and SUPPLIED-P-VAL are the function
;;; arguments.
;;; Essentially, we rewrite (lambda (&optional (x f x-p)) ...) as
;;; (lambda (&optional (x undef x-p)) (let ((x (if x-p x f))) ...)), but we
;;; perform this rewrite manually to avoid polluting the environment.
(defun process-init-parameter (var-cst var-val supplied-p-cst supplied-p-val
                               initform-cst inserter env system)
  (let* ((var-name (cst:raw var-cst))
         (var-info (env:variable-info system env var-name))
         (supplied-p-name
           (if supplied-p-cst (cst:raw supplied-p-cst) nil))
         (supplied-p-info
           (if supplied-p-cst
               (env:variable-info system env supplied-p-name)
               nil))
         (supplied-p-var
           (if supplied-p-cst
               (make-instance 'bir:variable :name supplied-p-name)
               nil))
         (supplied-p-test
           (if supplied-p-cst
               (make-instance 'bir:output :name supplied-p-name)
               supplied-p-val))
         (supplied-p-bind
           (if supplied-p-cst
               (make-instance 'bir:output :name supplied-p-name)
               nil))
         (block-supplied
           (make-iblock inserter :name (symbolicate var-name '#:-supplied)))
         (block-unsupplied
           (make-iblock inserter :name (symbolicate var-name '#:-unsupplied)))
         (block-merge
           (make-iblock inserter :name (symbolicate var-name '#:-merge)))
         (phi
           (make-instance 'bir:phi :iblock block-merge :name supplied-p-name)))
    (setf (bir:inputs block-merge) (list phi))
    ;; If there is a supplied-p variable in the code, we introduce our own
    ;; lexical variable for supplied-p because we need to use it twice: once
    ;; to see if we use the initform, and then once to bind the user variable.
    ;; The user variable might be special or something, so we don't use it
    ;; for this purpose.
    (when supplied-p-cst
      (adjoin-variable inserter supplied-p-var)
      (insert inserter 'bir:leti
              :inputs (list supplied-p-val) :outputs (list supplied-p-var))
      (insert inserter 'bir:readvar
              :inputs (list supplied-p-var) :outputs (list supplied-p-test)))
    ;; Test the suppliedp.
    (terminate inserter 'bir:ifi
               :inputs (list supplied-p-test)
               :next (list block-supplied block-unsupplied))
    ;; In the supplied block, just pass the parameter to the phi.
    (begin inserter block-supplied)
    (terminate inserter 'bir:jump
               :inputs (list var-val) :outputs (list phi)
               :next (list block-merge))
    ;; In the unsupplied block, compile the initform and use that value.
    (begin inserter block-unsupplied)
    (with-compiled-cst (iv initform-cst inserter env system)
      (terminate inserter 'bir:jump
                 :inputs iv :outputs (list phi)
                 :next (list block-merge)))
    ;; In the merge block, bind the source variable(s).
    (begin inserter block-merge)
    (bind-variable var-cst var-info (list phi) inserter system)
    (when supplied-p-cst
      (insert inserter 'bir:readvar
              :inputs (list supplied-p-var) :outputs (list supplied-p-bind))
      (bind-variable supplied-p-cst supplied-p-info
                     (list supplied-p-bind) inserter system)))
  (values))
