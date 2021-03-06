(in-package #:cleavir-bir)

(defgeneric verify (bir)
  ;; most-specific-last so that we get the core assertions out of the way;
  ;; failures there may cause hard to understand failures more specifically
  (:method-combination progn :most-specific-last))

(defgeneric verify-inputs (instruction))
(defgeneric verify-outputs (instruction))

(defclass problem ()
  ((%subject :initarg :subject :reader subject)
   (%format-control :initarg :format-control :reader problem-format-control)
   (%format-arguments :initarg :format-arguments
                      :reader problem-format-arguments)))

(defmethod print-object ((o problem) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "~a ~?" (subject o) (problem-format-control o)
              (problem-format-arguments o))))

(defvar *problems*)
(defmacro problem (format-control subject &rest format-arguments)
  `(push (make-instance 'problem
           :format-control ,format-control :subject ,subject
           :format-arguments (list ,@format-arguments))
         *problems*))
(defmacro test (condition format-control subject &rest format-arguments)
  `(unless ,condition
     (problem ,format-control ,subject ,@format-arguments)))

;;; KLUDGE: This is somewhat inexact; for example if two blocks share their
;;; only predecessor, one could have a computation in the other as an input,
;;; which is invalid, but if we happened to traverse the blocks in the wrong
;;; order we wouldn't catch it.
(defvar *seen-instructions*)

(defvar *seen-lists*)
(defvar *seen-next*)

;;; The module currently being verified.
(defvar *verifying-module*)
;;; " function
(defvar *verifying-function*)
;;; " iblock
(defvar *verifying-iblock*)

(defun member-of-lambda-list-p (argument lambda-list)
  (loop for l in lambda-list
        thereis (cond ((atom l) (eq argument l))
                      ((= (length l) 2)
                       (or (eq argument (first l))
                           (eq argument (second l))))
                      ((= (length l) 3)
                       (or (eq argument (second l))
                           (eq argument (third l)))))))

(defmethod verify progn ((instruction instruction))
  ;; verify type decls
  (test (typep (predecessor instruction) '(or instruction null))
        "has bad predecessor ~a" instruction (predecessor instruction))
  (test (typep (successor instruction) '(or instruction null))
        "has bad successor ~a" instruction (successor instruction))
  ;; We're our predecessor's successor and successor's predecessor
  (when (predecessor instruction)
    (test (eq (successor (predecessor instruction)) instruction)
          "is not its predecessor ~a's successor"
          instruction (predecessor instruction)))
  (when (successor instruction)
    (test (eq (predecessor (successor instruction)) instruction)
          "is not its successor ~a's predecessor"
          instruction (successor instruction)))
  ;; iblock is correct
  (test (eq (iblock instruction) *verifying-iblock*)
        "Instruction ~a's iblock ~a does not match its presence in ~a"
        instruction (iblock instruction) *verifying-iblock*)
  (verify-inputs instruction)
  (let ((inputs (inputs instruction)))
    (test (or (null inputs) (not (cleavir-set:presentp inputs *seen-lists*)))
          "has shared input list ~a" instruction inputs))
  (verify-outputs instruction)
  (let ((outputs (outputs instruction)))
    (test (or (null outputs) (not (cleavir-set:presentp outputs *seen-lists*)))
          "has shared output list ~a" instruction outputs)))

(defmethod verify-outputs ((instruction writevar))
  (let ((outputs (outputs instruction)))
    (test (and (= (length outputs) 1) (typep (first outputs) 'variable))
          "has bad outputs ~a" instruction outputs)
    (test (cleavir-set:presentp instruction (writers (first outputs)))
          "is not a definition of its output ~a"
          instruction (first outputs))))

(defun verify-phi-outputs (instruction)
  (let ((outputs (outputs instruction)))
    (flet ((phi-p (o) (typep o 'phi)))
      (test (every #'phi-p outputs)
            "has non-phi outputs ~a"
            instruction (remove-if #'phi-p outputs)))
    (flet ((presentp (o) (cleavir-set:presentp instruction (definitions o))))
      (test (every #'presentp outputs)
            "is not a definition of its outputs ~a"
            instruction (remove-if #'presentp outputs)))))

(defmethod verify-outputs ((instruction jump)) (verify-phi-outputs instruction))
(defmethod verify-outputs ((instruction unwind))
  (verify-phi-outputs instruction))

(defmethod verify-outputs ((instruction instruction))
  (let ((outputs (outputs instruction)))
    (flet ((outputp (o) (typep o 'output)))
      (test (every #'outputp outputs)
            "has outputs ~a of wrong class"
            instruction (remove-if #'outputp outputs)))
    (flet ((definerp (o) (eq instruction (definition o))))
      (test (every #'definerp outputs)
            "is not the definer of its outputs ~a"
            instruction (remove-if #'definerp outputs)))))

(defun linear-datum-already-defined-p (instruction datum)
  ;; All inputs are linear data, and if they are OUTPUTs, their definer
  ;; dominates this instruction (but see KLUDGE above)
  (etypecase datum
    (output (cleavir-set:presentp (definition datum) *seen-instructions*))
    (argument
     (member-of-lambda-list-p datum (lambda-list (function instruction))))
    ;; FIXME: Check PHI dominance too
    (linear-datum t)))

(defun check-ubd (instruction inputs)
  (let ((invalid (loop for inp in inputs
                       unless (linear-datum-already-defined-p instruction inp)
                         collect inp)))
    (test (null invalid)
          "has use-before-define on inputs ~a (of all inputs ~a)"
          instruction invalid inputs)))

(defun check-usedness (instruction inputs)
  (let ((invalid (loop for inp in inputs
                       unless (eq (use inp) instruction) collect inp)))
    (test (null invalid)
          "is not the use of its inputs ~a"
          instruction invalid)))

(defmethod verify-inputs ((instruction readvar))
  (let* ((inputs (inputs instruction)) (var (first inputs)))
    (test (and (= (length inputs) 1) (typep var 'variable))
          "has non-variable input ~a" instruction var)
    (test (cleavir-set:presentp instruction (readers var))
          "is not a reader of its variable ~a" instruction var)))

(defmethod verify-inputs ((instruction abstract-local-call))
  (let ((inputs (inputs instruction)))
    (test (typep (first inputs) 'function)
          "has non-function callee ~a"
          instruction (first inputs))
    (check-ubd instruction (rest inputs))
    (check-usedness instruction (rest inputs))))

(defmethod verify-inputs ((instruction constant-reference))
  (let* ((inputs (inputs instruction))
         (constant (first inputs)))
    (test (typep constant 'constant)
          "has non-constant input ~a"
          instruction constant)
    (test (cleavir-set:presentp constant (constants *verifying-module*))
          "references constant ~a which does not belong to its module"
          instruction constant)
    (test (cleavir-set:presentp instruction (readers constant))
          "is not a reader of its constant input ~a"
          instruction constant)))

(defmethod verify-inputs ((instruction load-time-value-reference))
  (let* ((inputs (inputs instruction))
         (ltv (first inputs)))
    (test (typep ltv 'load-time-value)
          "has non-LTV input ~a"
          instruction ltv)
    (test (cleavir-set:presentp ltv (load-time-values *verifying-module*))
          "references load-time-value ~a which does not belong to its module."
          instruction ltv)
    (test (cleavir-set:presentp instruction (readers ltv))
          "is not a reader of its LTV input ~a"
          instruction ltv)))

(defmethod verify-inputs ((instruction instruction))
  (check-ubd instruction (inputs instruction))
  (check-usedness instruction (inputs instruction)))

(defmethod verify progn ((instruction no-input))
  ;; No inputs (verify type decl)
  (test (null (inputs instruction))
        "has too many (more than zero) inputs ~a"
        instruction (inputs instruction)))

(defmethod verify progn ((instruction one-input))
  ;; verify type decl
  (test (and (listp (inputs instruction))
             (= (length (inputs instruction)) 1))
        "does not have exactly one input: ~a"
        instruction (inputs instruction)))

(defmethod verify progn ((instruction no-output))
  ;; verify type decl
  (test (null (outputs instruction))
        "has >0 outputs ~a" (outputs instruction)))

(defmethod verify progn ((instruction one-output))
  ;; verify type decl
  (test (and (listp (outputs instruction))
             (= (length (outputs instruction)) 1))
        "does not have exactly one output: ~a"
        instruction (outputs instruction)))

(defun match-jump-types (inst inputs outputs)
  ;; Ensure the number of inputs match that of the outputs
  (test (= (length inputs) (length outputs))
        "has mismatch between inputs ~a and outputs ~a"
        inst inputs outputs))

(defmethod verify progn ((instruction terminator))
  ;; No successor (verify type decl)
  (test (null (successor instruction))
        "has non-null successor ~a"
        instruction (successor instruction))
  ;; NEXT is a list of iblocks
  (flet ((iblockp (b) (typep b 'iblock)))
    (test (every #'iblockp (next instruction))
          "has non-iblock next ~a"
          instruction (remove-if #'iblockp (next instruction))))
  ;; NEXT list is not shared and therefore destructible
  (unless (null (next instruction))
    (test (not (cleavir-set:presentp (next instruction) *seen-next*))
          "shares its next-list ~a"
          instruction (next instruction))
    (cleavir-set:nadjoinf *seen-next* (next instruction))))

(defmethod verify progn ((instruction terminator0))
  ;; No NEXT (verify type decl)
  (test (null (next instruction))
        "should have 0 next but has ~a"
        instruction (next instruction)))

(defmethod verify progn ((instruction terminator1))
  ;; verify type decl
  (test (= (length (next instruction)) 1)
        "should have 1 next but has ~a"
        instruction (next instruction)))

(defmethod verify progn ((inst enclose))
  ;; verify type decls
  (test (typep (code inst) 'function)
        "has bad code ~a" inst (code inst))
  ;; Make sure enclose is correct
  (test (eq inst (enclose (code inst)))
        "is not its CODE's ~a enclose ~a."
        inst (code inst) (enclose (code inst)))
  ;; Make sure the function we are enclosing is in the module.
  (when (boundp '*verifying-module*)
    (test (cleavir-set:presentp (code inst) (functions *verifying-module*))
          "encloses ~a, which is not present in the module ~a"
          inst (code inst) *verifying-module*)))

(defmethod verify progn ((inst abstract-local-call))
  ;; Make sure the function we are calling is in the module.
  (when (boundp '*verifying-module*)
    (let ((function (first (inputs inst))))
      (test (cleavir-set:presentp function (functions *verifying-module*))
            "calls ~a, which is not present in the module ~a"
            inst function *verifying-module*))))

(defun dynenvs (d)
  (loop for dyn = d then (parent dyn)
        collect dyn
        until (typep dyn 'function)))

(defmethod verify progn ((rv readvar))
  (let ((var (first (inputs rv))))
    ;; make sure something writes the variable
    (test (plusp (cleavir-set:size (writers var)))
          "reads variable ~a with no writers" rv var)))

(defmethod verify progn ((call call))
  (test (> (length (inputs call)) 0)
        "is missing a callee" call))

(defmethod verify progn ((c catch))
  ;; verify type decls
  (test (typep (unwinds c) 'cleavir-set:set)
        "has non-set for unwinds: ~a" c (unwinds c))
  ;; check that it is recorded by its function
  (test (cleavir-set:presentp c (catches (function c)))
        "not in its function ~a's catch set." c (function c))
  ;; check that all unwinds are unwinds
  (let ((non-unwinds
          (cleavir-set:filter
           'list (lambda (u) (not (typep u 'unwind))) (unwinds c))))
    (test (null non-unwinds)
          "has non-unwinds ~a in its unwind set"
          c non-unwinds))
  ;; check that there's at least one next
  (test (> (length (next c)) 0)
        "has no nexts" c)
  ;; Check that the normal next has this dynamic environment
  (test (eq (dynamic-environment (first (next c))) c)
        "has normal successor ~a with wrong dynamic environment ~a"
        c (first (next c)) (dynamic-environment (first (next c)))))

(defmethod verify progn ((u unwind))
  ;; verify type decls
  (test (typep (catch u) 'catch)
        "has \"catch\" ~a, which is not a catch" u (catch u))
  (test (typep (destination u) 'iblock)
        "has destination ~a, which is not an iblock" u (destination u))
  ;; Make sure the catch knows about us
  ;; (since if we're being verified, we must be reachable and live)
  (test (cleavir-set:presentp u (unwinds (catch u)))
        "is not present in its catch's ~a unwinds ~a"
        u (catch u) (unwinds (catch u)))
  ;; Make sure this unwind's block is an entrance of the destination block.
  (test (cleavir-set:presentp (iblock u) (entrances (destination u)))
        "has iblock ~a, which is not in the entrances set of the destination ~a"
        u (iblock u) (destination u))
  ;; ensure inputs match destination
  (match-jump-types u (inputs u) (outputs u)))

(defmethod verify progn ((j jump))
  (match-jump-types j (inputs j) (outputs j))
  ;; Check accuracy of unwindp (TODO: Check that the dynenv is a parent)
  ;; (probably that's a general thing for terminators tho)
  (let ((de (dynamic-environment *verifying-iblock*)))
    (unless (unwindp j)
      (test (eq de (dynamic-environment (first (next j))))
            "is not marked as an unwind, but does unwind" j))))

(defmethod verify progn ((instruction conditional-test))
  ;; Verify that the destination is an IFI.
  (test (typep (use (output instruction)) '(or null ifi))
        "is used by ~a, not an ifi instruction"
        instruction (use (output instruction))))

(defmethod verify progn ((iblock iblock))
  ;; All predecessors truly have this as a successor
  (let ((non-successor-predecessors
          (cleavir-set:filter 'list
                              (lambda (p) (not (member iblock (next (end p))
                                                       :test #'eq)))
                              (predecessors iblock))))
    (test (null non-successor-predecessors)
          "has predecessors which do not list it as a successor:~%~a"
          iblock non-successor-predecessors))
  ;; All successors have this as a predecessor
  (flet ((has-predecessor-p (next)
           (cleavir-set:presentp iblock (predecessors next))))
    (test (every #'has-predecessor-p (next (end iblock)))
          "has successors which do not list it as a predecessor:~%~a"
          iblock
          (remove-if #'has-predecessor-p (next (end iblock)))))
  ;; Start is an instruction (verify type decl)
  (test (typep (start iblock) 'instruction)
        "has non-instruction start ~a" iblock (start iblock))
  ;; Start instruction has no predecessor
  (test (null (predecessor (start iblock)))
        "has start instruction ~a with non-null predecessor"
        iblock (predecessor (start iblock)))
  ;; End instruction is a terminator (verify type decl)
  (test (typep (end iblock) 'terminator)
        "has final instruction ~a, which is not a terminator"
        iblock (end iblock))
  ;; Dynenv is a dynenv (verify type decl)
  (test (typep (dynamic-environment iblock) 'dynamic-environment)
        "has dynamic environment ~a, which is not a dynamic-environment"
        iblock (dynamic-environment iblock))
  ;; iblock is in its dynenv's scope set
  (test (cleavir-set:presentp iblock (scope (dynamic-environment iblock)))
        "is not in its dynamic environment ~a's scope"
        iblock (dynamic-environment iblock))
  ;; dynenv is either the function itself or an instruction that
  ;; dominates this block (but see KLUDGE above)
  (test (or (eq (dynamic-environment iblock) *verifying-function*)
            (cleavir-set:presentp
             (dynamic-environment iblock) *seen-instructions*))
        "has invalid dynamic environment ~a"
        iblock (dynamic-environment iblock))
  (test (function iblock)
        "has no function; it may have been deleted." iblock)
  ;; Function is the right function
  (test (eq (function iblock) *verifying-function*)
        "is in the wrong function." iblock)
  ;; Check entrances actually end in unwind.
  (cleavir-set:doset (entrance (entrances iblock))
    (test (typep (end entrance) 'unwind)
          "has entrance ~a, which does not end in an unwind" iblock entrance))
  ;; inputs are all phis, and all phis have only terminators as definitions
  (let ((inputs (inputs iblock)))
    (flet ((phip (p) (typep p 'phi)))
      (test (every #'phip inputs)
            "has non-phi inputs ~a" iblock (remove-if #'phip inputs)))
    (flet ((terminatord (p)
             (cleavir-set:every (lambda (inst)
                                  (and (typep inst 'terminator)
                                       (member p (outputs inst))))
                                (definitions p))))
      (test (every #'terminatord inputs)
            "has input phis ~a, which have some invalid definitions"
            iblock (remove-if #'terminatord inputs)))
    ;; Inputs lists are not shared, so we can destroy them
    (unless (null inputs)
      (test (not (cleavir-set:presentp inputs *seen-lists*))
            "has shared inputs list" iblock)
      (cleavir-set:nadjoinf *seen-lists* inputs)))
  ;; Verify each instruction
  (let ((*verifying-iblock* iblock))
    (do-iblock-instructions (i iblock)
      ;; Ensure it's actually an instruction
      (test (typep i 'instruction)
            "is in an iblock despite not being an instruction" i)
      ;; Ensure each instruction is only in the graph once
      (test (not (cleavir-set:presentp i *seen-instructions*))
            "is in the graph multiple times" i)
      ;; Ensure non-end instructions are non-terminator instructions
      (unless (eq i (end iblock))
        (test (not (typep i 'terminator))
              "does not terminate its iblock" i))
      (verify i)
      (cleavir-set:nadjoinf *seen-instructions* i))))

(defmethod verify progn ((function function))
  (let ((start (start function))
        (returni (returni function))
        (*verifying-function* function))
    ;; make sure the function is actually in its module.
    (test (cleavir-set:presentp function (functions (module function)))
          "is not present in its module"
          function)
    ;; start is an iblock (verify type decl)
    (test (typep start 'iblock)
          "has start ~a, which is not an iblock" function start)
    ;; Module is correct
    (when (boundp '*verifying-module*)
      (test (eq (module function) *verifying-module*)
            "in the wrong module" function))
    ;; Reachability etc
    (let ((reachable (cleavir-set:empty-set))
          (iblocks (cleavir-set:empty-set)))
      (do-iblocks (iblock function)
        (test (not (cleavir-set:presentp iblock iblocks))
              "is present in the iblock flow order of function ~a more than once."
              iblock function)
        (cleavir-set:nadjoinf iblocks iblock))
      (labels ((iblock-verifier (iblock)
                 (verify iblock)
                 ;; A function has at most one return instruction
                 (test (if (typep (end iblock) 'returni)
                           (eq (end iblock) returni)
                           t)
                       "ends in a returni which is not the returni of its function."
                       iblock)
                 (cleavir-set:nadjoinf reachable iblock))
               (traverse (iblock)
                 (unless (cleavir-set:presentp iblock reachable)
                   (cleavir-set:nadjoinf reachable iblock)
                   (iblock-verifier iblock)
                   (dolist (successor (successors iblock))
                     (traverse successor)))))
        (traverse start)
        ;; All reachable blocks are in the iblocks set
        (test (cleavir-set:set<= reachable iblocks)
              "does not record reachable iblocks ~a"
              function (cleavir-set:difference 'list reachable iblocks))
        ;; All members of the iblocks set are reachable
        (test (cleavir-set:set<= iblocks reachable)
              "records unreachable iblocks ~a"
              function (cleavir-set:difference 'list iblocks reachable)))
      ;; Check that the catch instructions of this function were in
      ;; fact seen.
      (cleavir-set:doset (catch (catches function))
        (test (cleavir-set:presentp catch *seen-instructions*)
              "is recorded by the function ~a but is not reachable."
              catch function))
      ;; The return instruction's iblock, if it exists, is reachable
      ;; and in the iblocks set.
      (when returni
        (let ((end (iblock returni)))
          (test (cleavir-set:presentp end reachable)
                "has unreachable return iblock"
                function)
          (test (cleavir-set:presentp end iblocks)
                "has unrecorded return iblock"
                function))))))

(defmethod verify progn ((module module))
  (let ((*problems* nil) (function-problems nil))
    (handler-case
        (let ((*seen-instructions* (cleavir-set:empty-set))
              (*seen-lists* (cleavir-set:empty-set))
              (*seen-next* (cleavir-set:empty-set))
              (*verifying-module* module))
          (do-functions (function module)
            (let ((*problems* nil))
              (verify function)
              (when *problems*
                (push (list* function *problems*) function-problems))))
          ;; Now do module-level tests
          ;; Check for dangling references.
          (cleavir-set:doset (constant (constants module))
            (test (not (cleavir-set:empty-set-p (readers constant)))
                  "records a constant with no references ~a." module constant))
          (cleavir-set:doset (ltv (load-time-values module))
            (test (not (cleavir-set:empty-set-p (readers ltv)))
                  "records an LTV with no references ~a." module ltv)))
      (error (e)
        (error 'verification-error :module module :original-condition e)))
    ;; Report results.
    (when (or function-problems *problems*)
      (error 'verification-failed :module module
                                  :function-problems function-problems
                                  :module-problems *problems*))))
