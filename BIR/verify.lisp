(in-package #:cleavir-bir)

(defgeneric verify (bir)
  ;; most-specific-last so that we get the core assertions out of the way;
  ;; failures there may cause hard to understand failures more specifically
  (:method-combination progn :most-specific-last))

(defvar *problems*)
(defmacro problem (format-control &rest format-arguments)
  `(push (list ,format-control (list ,@format-arguments)) *problems*))
(defmacro test (condition format-control &rest format-arguments)
  `(unless ,condition (problem ,format-control ,@format-arguments)))

(defmacro with-problems ((function) &body body)
  (let ((gfunction (gensym "FUNCTION")))
    `(let ((*problems* nil) (,gfunction ,function))
       (handler-case
           (progn ,@body)
         (error (e)
           (error "BUG: Error while verifying function ~a:~%~t~a~%"
                  (name ,gfunction) e)))
       (unless (null *problems*)
         (error "Problems found in function ~a!~%~:{~&~?~%~}"
                (name ,gfunction) *problems*))
       (values))))

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
        "Instruction ~a has bad predecessor ~a"
        instruction (predecessor instruction))
  (test (typep (successor instruction) '(or instruction null))
        "Instruction ~a has bad successor ~a")
  ;; We're our predecessor's successor and successor's predecessor
  (when (predecessor instruction)
    (test (eq (successor (predecessor instruction)) instruction)
          "Instruction ~a is not its predecessor ~a's successor"
          instruction (predecessor instruction)))
  (when (successor instruction)
    (test (eq (predecessor (successor instruction)) instruction)
          "Instruction ~a is not its successor ~a's predecessor"
          instruction (successor instruction)))
  ;; iblock is correct
  (test (eq (iblock instruction) *verifying-iblock*)
        "Instruction ~a's iblock ~a does not match its presence in ~a"
        instruction (iblock instruction) *verifying-iblock*)
  ;; All inputs are LINEAR-DATUMs, and if they're instructions, they dominate
  ;; this instruction (but see KLUDGE above).
  (let ((inputs (inputs instruction)))
    (typecase instruction
      (readvar
       (test (and (= (length inputs) 1)
                  (typep (first inputs) 'variable))
             "Accessvar ~a has non-variable input ~a"
             instruction (first inputs)))
      (local-call
       (assert (typep (first inputs) 'function))
       (assert (every (lambda (i)
                        (eq (use i) instruction))
                      (rest inputs))))
      (mv-local-call
       (assert (typep (first inputs) 'function)))
      (constant-reference
       (test (typep (first inputs) 'constant)
             "Constant reference ~a has non-constant input ~a"
             instruction (first inputs))
       (test (cleavir-set:presentp (first inputs) (constants *verifying-module*))
             "Referenced constant ~a not in its module."
             (first inputs)))
      (t (flet ((validp (v)
                  (etypecase v
                    (computation (cleavir-set:presentp v *seen-instructions*))
                    (argument
                     (member-of-lambda-list-p
                      v
                      (lambda-list (function instruction))))
                    (linear-datum t))))
           (test (every #'validp inputs)
                 "Instruction ~a, with inputs ~a,
has use-before-define on inputs ~a"
                 instruction inputs
                 (remove-if #'validp inputs)))
       (flet ((used-input-p (input)
                (eq (use input) instruction)))
         (test (every #'used-input-p inputs)
               "Instruction ~a is not the use of its inputs ~a"
               instruction
               (remove-if #'used-input-p inputs)))))
    ;; Make sure input lists are not shared, so we can destroy them
    (unless (null inputs)
      (test (not (cleavir-set:presentp inputs *seen-lists*))
            ;; could track the other instruction sharing it
            "Inputs list of instruction ~a is shared" instruction)
      (cleavir-set:nadjoinf *seen-lists* inputs))))

(defmethod verify progn ((instruction operation))
  ;; All outputs are OUTPUTs, unless this is a terminator, in which case
  ;; they're all PHIs, or unless this is a WRITEVAR.
  ;; In either case, we're a definer.
  (let ((outputs (outputs instruction)))
    (typecase instruction
      (writevar
       (test (and (= (length outputs) 1) (typep (first outputs) 'variable))
             "Writevar ~a has bad outputs ~a" instruction outputs)
       (test (cleavir-set:presentp instruction (writers (first outputs)))
             "Writevar ~a is not a definition of its output ~a"
             instruction (first outputs)))
      (terminator
       (flet ((phi-p (o) (typep o 'phi)))
         (test (every #'phi-p outputs)
               "Terminator ~a has non-phi outputs ~a"
               instruction (remove-if #'phi-p outputs)))
       (flet ((presentp (o) (member instruction (definitions o))))
         (test (every #'presentp outputs)
               "Terminator ~a is not a definition of its outputs ~a"
               instruction (remove-if #'presentp outputs))))
      (t
       (flet ((outputp (o) (typep o 'output)))
         (test (every #'outputp outputs)
               "Operation ~a has outputs ~a of wrong class"
               instruction (remove-if #'outputp outputs)))
       (flet ((definerp (o) (eq instruction (definition o))))
         (test (every #'definerp outputs)
               "Operation ~a is not the definer of its outputs ~a"
               instruction (remove-if #'definerp outputs)))))
    ;; Make sure output lists are not shared.
    (unless (null outputs)
      (test (not (cleavir-set:presentp outputs *seen-lists*))
            "Instruction ~a has shared output list ~a" instruction outputs))))

(defmethod verify progn ((instruction no-input))
  ;; No inputs (verify type decl)
  (test (null (inputs instruction))
        "No-input instruction ~a has >0 inputs ~a"
        instruction (inputs instruction)))

(defmethod verify progn ((instruction one-input))
  ;; verify type decl
  (test (and (listp (inputs instruction))
             (= (length (list (inputs instruction))) 1))
        "One-input instruction ~a does not have exactly one input: ~a"
        instruction (inputs instruction)))

(defmethod verify progn ((instruction no-output))
  ;; verify type decl
  (test (null (outputs instruction))
        "No-output instruction ~a has >0 outputs ~a" (outputs instruction)))

(defun match-jump-types (inst inputs outputs)
  ;; Ensure the number and rtypes of the inputs match those of the outputs
  (test (= (length inputs) (length outputs))
        "Jump/unwind ~a has mismatch between inputs ~a and outputs ~a"
        inst inputs outputs)
  (flet ((rt= (x y) (rtype= (rtype x) (rtype y))))
    (test (every #'rt= inputs outputs)
          "Jump/unwind ~a has mismatched rtypes between inputs and outputs"
          inst)))

(defmethod verify progn ((instruction terminator))
  ;; No successor (verify type decl)
  (test (null (successor instruction))
        "Terminator ~a has non-null successor ~a"
        instruction (successor instruction))
  ;; NEXT is a list of iblocks
  (flet ((iblockp (b) (typep b 'iblock)))
    (test (every #'iblockp (next instruction))
          "Terminator ~a has non-iblock next ~a"
          instruction (remove-if #'iblockp (next instruction))))
  ;; NEXT list is not shared and therefore destructible
  (unless (null (next instruction))
    (test (not (cleavir-set:presentp (next instruction) *seen-next*))
          "NEXT is shared for instruction: ~a" (next instruction))
    (cleavir-set:nadjoinf *seen-next* (next instruction))))

(defmethod verify progn ((instruction terminator0))
  ;; No NEXT (verify type decl)
  (test (null (next instruction))
        "Terminator ~a should have 0 next but has ~a"
        instruction (next instruction)))

(defmethod verify progn ((instruction terminator1))
  ;; verify type decl
  (test (= (length (next instruction)) 1)
        "Terminator ~a should have 1 next but has ~a"
        instruction (next instruction)))

(defmethod verify progn ((inst enclose))
  ;; verify type decls
  (test (typep (code inst) 'function)
        "Enclose ~a has bad code ~a" inst (code inst))
  ;; Make sure enclose is correct
  (test (eq inst (enclose (code inst)))
        "Enclose ~a is not its CODE's ~a enclose ~a."
        inst (code inst) (enclose (code inst)))
  ;; Make sure the function we are enclosing is in the module.
  (when (boundp '*verifying-module*)
    (test (cleavir-set:presentp (code inst) (functions *verifying-module*))
          "The function ~a being enclosed by ~a is not present in the module ~a."
          (code inst) inst *verifying-module*)))

(defmethod verify progn ((inst abstract-local-call))
  ;; Make sure the function we are calling is in the module.
  (when (boundp '*verifying-module*)
    (let ((function (first (inputs inst))))
      (test (cleavir-set:presentp function (functions *verifying-module*))
            "The function ~a being called by ~a is not present in the module ~a."
            function inst *verifying-module*))))

(defun dynenvs (d)
  (loop for dyn = d then (parent dyn)
        collect dyn
        until (typep dyn 'function)))

(defmethod verify progn ((at accesstemp))
  ;; verify type decl
  (test (typep (alloca at) 'alloca)
        "Accesstemp ~a has non-alloca ALLOCA slot: ~a"
        at (alloca at))
  ;; Check that the alloca is an ancestor of the dynenv
  (test (loop with alloca = (alloca at)
              for dyn = (dynamic-environment at)
                then (parent dyn)
              when (eq dyn alloca)
                return t
              finally (return nil))
        "Accesstemp ~a's alloca is not an ancestor of its dynamic environment: ~a"
        at (dynenvs (dynamic-environment at))))

(defmethod verify progn ((wv writevar))
  ;; match types
  (test (rtype= (rtype (first (inputs wv)))
                (rtype (first (outputs wv))))
        "Writevar ~a has input rtype ~a but output rtype ~a"
        wv (rtype (first (inputs wv))) (rtype (first (outputs wv)))))

(defmethod verify progn ((rv readvar))
  (let ((var (first (inputs rv))))
    ;; make sure something writes the variable
    (test (plusp (cleavir-set:size (writers var)))
          "Readvar ~a reads variable ~a with no writers"
          rv var)))

(defmethod verify progn ((call call))
  (test (> (length (inputs call)) 0)
        "Call ~a is missing a callee" call))

(defmethod verify progn ((c catch))
  ;; verify type decls
  (test (typep (unwinds c) 'cleavir-set:set)
        "Catch ~a has non-set for unwinds: ~a" c (unwinds c))
  ;; check that it is recorded by its function
  (test (cleavir-set:presentp c (catches (function c)))
        "Catch ~a not in its function ~a's catch set." c (function c))
  ;; check that all unwinds are unwinds
  (test (cleavir-set:every (lambda (u) (typep u 'unwind)) (unwinds c))
        "Catch ~a has non-unwinds ~a in its unwind set"
        c (cleavir-set:filter 'list (lambda (u) (not (typep u 'unwind)))
                              (unwinds c)))
  ;; check that there's at least one next
  (test (> (length (next c)) 0)
        "Catch ~a has no nexts" c)
  ;; Check that the normal next has this dynamic environment
  (test (eq (dynamic-environment (first (next c))) c)
        "Catch ~a has normal successor ~a with wrong dynamic environment ~a"
        c (first (next c)) (dynamic-environment (first (next c)))))

(defmethod verify progn ((u unwind))
  ;; verify type decls
  (test (typep (catch u) 'catch)
        "Unwind ~a's catch ~a is not a catch" u (catch u))
  (test (typep (destination u) 'iblock)
        "Unwind ~a's destination ~a is not an iblock" u (destination u))
  ;; Make sure the catch knows about us
  ;; (since if we're being verified, we must be reachable and live)
  (test (cleavir-set:presentp u (unwinds (catch u)))
        "Unwind ~a is not present in its catch's ~a unwinds ~a"
        u (catch u) (unwinds (catch u)))
  ;; Make sure this unwind's block is an entrance of the destination block.
  (test (cleavir-set:presentp (iblock u) (entrances (destination u)))
        "The iblock of unwind ~a is not in the entrances set of the destination ~a"
        u (destination u))
  ;; ensure inputs match destination
  (match-jump-types u (inputs u) (outputs u)))

(defmethod verify progn ((j jump))
  (match-jump-types j (inputs j) (outputs j))
  ;; Check accuracy of unwindp (TODO: Check that the dynenv is a parent)
  ;; (probably that's a general thing for terminators tho)
  (let ((de (dynamic-environment *verifying-iblock*)))
    (unless (unwindp j)
      (test (eq de (dynamic-environment (first (next j))))
            "Jump ~a is not marked as an unwind, but does unwind" j))))

(defmethod verify progn ((instruction conditional-test))
  ;; Verify that the destination is an IFI.
  (test (typep (use instruction) '(or null ifi))
        "conditional test ~a is not used by an ifi instruction" instruction))

(defmethod verify progn ((mtf multiple-to-fixed))
  (test (rtype= (rtype (first (inputs mtf))) :multiple-values)
        "MTF ~a's first input's rtype is ~a, not ~a"
        mtf (rtype (first (inputs mtf))) :multiple-values))

(defmethod verify progn ((iblock iblock))
  ;; All predecessors truly have this as a successor
  (let ((non-successor-predecessors
          (cleavir-set:filter 'list
                              (lambda (p) (not (member iblock (next (end p))
                                                       :test #'eq)))
                              (predecessors iblock))))
    (test (null non-successor-predecessors)
          "Some predecessors to iblock ~a do not list it as a successor:~%~a"
          iblock non-successor-predecessors))
  ;; All successors have this as a predecessor
  (flet ((has-predecessor-p (next)
           (cleavir-set:presentp iblock (predecessors next))))
    (test (every #'has-predecessor-p (next (end iblock)))
          "Some successors to iblock ~a do not list it as a predecessor:
~a"
          iblock
          (remove-if #'has-predecessor-p (next (end iblock)))))
  ;; Start is an instruction (verify type decl)
  (test (typep (start iblock) 'instruction)
        "Iblock ~a has non-instruction start ~a" iblock (start iblock))
  ;; Start instruction has no predecessor
  (test (null (predecessor (start iblock)))
        "iblock ~a's start instruction has non-null predecessor ~a"
        iblock (predecessor (start iblock)))
  ;; End instruction is a terminator (verify type decl)
  (test (typep (end iblock) 'terminator)
        "iblock ~a's final instruction ~a is not a terminator"
        iblock (end iblock))
  ;; Dynenv is a dynenv (verify type decl)
  (test (typep (dynamic-environment iblock) 'dynamic-environment)
        "iblock ~a's dynamic environment ~a is not a dynamic-environment"
        iblock (dynamic-environment iblock))
  ;; iblock is in its dynenv's scope set
  (test (cleavir-set:presentp iblock (scope (dynamic-environment iblock)))
        "iblock ~a is not in its dynamic environment ~a's scope"
        iblock (dynamic-environment iblock))
  ;; dynenv is either the function itself or an instruction that
  ;; dominates this block (but see KLUDGE above)
  (test (or (eq (dynamic-environment iblock) *verifying-function*)
            (cleavir-set:presentp
             (dynamic-environment iblock) *seen-instructions*))
        "iblock ~a has invalid dynamic environment ~a"
        iblock (dynamic-environment iblock))
  ;; Function is the right function
  (test (eq (function iblock) *verifying-function*)
        "iblock ~a is in the wrong function"
        iblock)
  ;; Check entrances actually end in unwind.
  (cleavir-set:doset (entrance (entrances iblock))
    (test (typep (end entrance) 'unwind)
          "entrance ~a of iblock ~a does not end in unwind"
          entrance iblock))
  ;; inputs are all phis, and all phis have only terminators as definitions
  (flet ((phip (p) (typep p 'phi)))
    (test (every #'phip (inputs iblock))
          "iblock ~a has non-phi inputs ~a"
          iblock (remove-if #'phip (inputs iblock))))
  (flet ((terminatord (p)
           (every (lambda (inst)
                    (and (typep inst 'terminator)
                         (member p (outputs inst))))
                  (definitions p))))
    (test (every #'terminatord (inputs iblock))
          "phis ~a have some invalid definitions"
          (remove-if #'terminatord (inputs iblock))))
  ;; Verify each instruction
  (let ((*verifying-iblock* iblock))
    (do-iblock-instructions (i (start iblock))
      ;; Ensure each instruction is only in the graph once
      (test (not (cleavir-set:presentp i *seen-instructions*))
            "Instruction ~a is in the graph multiple times" i)
      ;; Ensure non-end instructions are non-terminator instructions
      (unless (eq i (end iblock))
        (test (typep i '(and instruction (not terminator)))
              "~a is not a non-terminator instruction" i))
      (verify i)
      (cleavir-set:nadjoinf *seen-instructions* i))))

(defmethod verify progn ((function function))
  (with-problems (function)
    (let ((start (start function))
          (returni (returni function))
          (*verifying-function* function))
      ;; make sure the function is actually in its module.
      (test (cleavir-set:presentp function (functions (module function)))
            "Locally referenced or called function ~a not present in its module."
            function)
      ;; start is an iblock (verify type decl)
      (test (typep start 'iblock)
            "Function start ~a is not an iblock" start)
      ;; Module is correct
      (when (boundp '*verifying-module*)
        (test (eq (module function) *verifying-module*)
              "Function ~a is in the wrong module" function))
      ;; Reachability etc
      (let ((reachable (cleavir-set:empty-set))
            (iblocks (cleavir-set:empty-set)))
        (do-iblocks (iblock function)
          (test (not (cleavir-set:presentp iblock iblocks))
                "Iblock ~a is present in the iblock flow order of function ~a more than once."
                iblock function)
          (cleavir-set:nadjoinf iblocks iblock))
        (labels ((iblock-verifier (iblock)
                   (verify iblock)
                   ;; A function has at most one return instruction
                   (test (if (typep (end iblock) 'returni)
                             (eq (end iblock) returni)
                             t)
                         "iblock ~a ends in a returni which is not the returni of the function."
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
                "Some reachable iblocks ~a are not recorded by the function ~a"
                (cleavir-set:difference 'list reachable iblocks)
                function)
          ;; All members of the iblocks set are reachable
          (test (cleavir-set:set<= iblocks reachable)
                "Some iblocks recorded by the function ~a are unreachable: ~a"
                function
                (cleavir-set:difference 'list iblocks reachable)))
        ;; Check that the catch instructions of this function were in
        ;; fact seen.
        (cleavir-set:doset (catch (catches function))
          (test (cleavir-set:presentp catch *seen-instructions*)
                "The catch ~a is recorded by the function ~a but not reachable." catch function))
        ;; The return instruction's iblock, if it exists, is reachable
        ;; and in the iblocks set.
        (when returni
          (let ((end (iblock returni)))
            (test (cleavir-set:presentp end reachable)
                  "The return iblock of the function ~a is not reachable."
                  function)
            (test (cleavir-set:presentp end iblocks)
                  "The return iblock of function ~a is not recorded."
                  function)))))))

(defmethod verify progn ((module module))
  (let ((*seen-instructions* (cleavir-set:empty-set))
        (*seen-lists* (cleavir-set:empty-set))
        (*seen-next* (cleavir-set:empty-set))
        (*verifying-module* module))
    (cleavir-set:mapset nil #'verify (functions module))))
