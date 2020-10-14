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
  ;; this instruction (but see KLUDGE above). READVAR has a non-linear input.
  ;; Also check that the uses are hooked up.
  (let ((inputs (inputs instruction)))
    (typecase instruction
      (readvar
       (test (and (= (length inputs) 1)
                  (typep (first inputs) 'variable))
             "Readvar ~a has non-variable input ~a"
             instruction (first inputs))
       (test (cleavir-set:presentp instruction (uses (first inputs)))
             "Readvar ~a is not in the uses of its input variable ~a"
             instruction (first inputs)))
      (local-call
       (assert (typep (first inputs) 'function))
       (assert (every (lambda (i) (eq (use i) instruction)) (rest inputs))))
      (t (flet ((validp (v)
                  (etypecase v
                    (computation (cleavir-set:presentp v *seen-instructions*))
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
      ((or catch writevar)
       (test (and (= (length outputs) 1) (typep (first outputs) 'variable))
             "Writevar ~a has bad outputs ~a" instruction outputs)
       (test (cleavir-set:presentp instruction
                                   (definitions (first outputs)))
             "Writevar ~a is not a definition of its output ~a"
             instruction (first outputs)))
      (terminator
       (flet ((phi-p (o) (typep o 'phi)))
         (test (every #'phi-p outputs)
               "Terminator ~a has non-phi outputs ~a"
               instruction (remove-if #'phi-p outputs)))
       (flet ((presentp (o) (cleavir-set:presentp instruction (definitions o))))
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
  ;; Make sure encloses set is correct
  (test (cleavir-set:presentp inst (encloses (code inst)))
        "Enclose ~a is not present in its CODE ~a's encloses"
        inst (code inst) (encloses (code inst)))
  ;; Make sure the function we are enclosing is in the module.
  (test (cleavir-set:presentp (code inst) (functions *verifying-module*))
        "The function ~a being enclosed by ~a is not present in the module ~a."
        (code inst) inst *verifying-module*))

(defmethod verify progn ((inst local-call))
  ;; Make sure the function we are calling is in the module.
  (let ((function (first (inputs inst))))
    (test (cleavir-set:presentp function (functions *verifying-module*))
          "The function ~a being called by ~a is not present in the module ~a."
          function inst *verifying-module*)))

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
    (test (not (cleavir-set:empty-set-p (definitions var)))
          "Readvar ~a reads variable ~a with no writers"
          rv (first (inputs rv)))))

(defmethod verify progn ((call call))
  (test (> (length (inputs call)) 0)
        "Call ~a is missing a callee" call))

(defmethod verify progn ((c catch))
  ;; verify type decls
  (test (typep (unwinds c) 'cleavir-set:set)
        "Catch ~a has non-set for unwinds: ~a" c (unwinds c))
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
        "Unwind ~a's destinaction ~a is not an iblock" u (destination u))
  ;; Make sure the catch knows about us
  ;; (since if we're being verified, we must be reachable and live)
  (test (cleavir-set:presentp u (unwinds (catch u)))
        "Unwind ~a is not present in its catch's ~a unwinds ~a"
        u (catch u) (unwinds (catch u)))
  ;; ensure there is at least one input (the continuation)
  (test (> (length (inputs u)) 0)
        "Unwind ~a is missing inputs" u)
  ;; ensure the first input is a continuation
  (test (rtype= (rtype (first (inputs u))) :continuation)
        "Unwind ~a's first input ~a is not a continuation"
        u (first (inputs u)))
  ;; ensure inputs match destination
  (match-jump-types u (rest (inputs u)) (outputs u)))

(defmethod verify progn ((j jump))
  (match-jump-types j (inputs j) (outputs j))
  ;; Check accuracy of unwindp (TODO: Check that the dynenv is a parent)
  ;; (probably that's a general thing for terminators tho)
  (let ((de (dynamic-environment *verifying-iblock*)))
    (unless (unwindp j)
      (test (eq de (dynamic-environment (first (next j))))
            "Jump ~a is not marked as an unwind, but does unwind" j))))

(defmethod verify progn ((eqi eqi))
  ;; Verify next count
  (test (= (length (next eqi)) 2)
        "Eqi ~a has != 2 next ~a" eqi (next eqi)))

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
        "iblock ~a is in the wrong function")
  ;; inputs are all phis, and all phis have only terminators as definitions
  (flet ((phip (p) (typep p 'phi)))
    (test (every #'phip (inputs iblock))
          "iblock ~a has non-phi inputs ~a"
          iblock (remove-if #'phip (inputs iblock))))
  (flet ((terminatord (p)
           (cleavir-set:every (lambda (inst)
                                (typep inst 'terminator))
                              (definitions p))))
    (test (every #'terminatord (inputs iblock))
          "phis ~a have non-terminator definitions"
          (remove-if #'terminatord (inputs iblock))))
  ;; Verify each instruction
  (let ((*verifying-iblock* iblock))
    (map-iblock-instructions
     (lambda (i)
       ;; Ensure each instruction is only in the graph once
       (test (not (cleavir-set:presentp i *seen-instructions*))
             "Instruction ~a is in the graph multiple times" i)
       ;; Ensure non-end instructions are non-terminator instructions
       (unless (eq i (end iblock))
         (test (typep i '(and instruction (not terminator)))
               "~a is not a non-terminator instruction" i))
       (verify i)
       (cleavir-set:nadjoinf *seen-instructions* i))
     (start iblock))))

(defmethod verify progn ((function function))
  (with-problems (function)
    (let ((start (start function))
          (end (end function))
          (*verifying-function* function))
      ;; make sure the function is actually in its module.
      (test (cleavir-set:presentp function (functions (module function)))
            "Locally referenced or called function ~a not present in its module."
            function)
      ;; start is an iblock (verify type decl)
      (test (typep start 'iblock)
            "Function start ~a is not an iblock" start)
      ;; end is an iblock or nil (verify type decl)
      (test (typep end '(or iblock null))
            "Function end ~a is not an iblock or NIL" end)
      ;; End of the end block is a return instruction
      (when end
        (test (and (slot-boundp end '%end) (typep (end end) 'returni))
              "Final instruction ~a is not a returni" (end end)))
      ;; Module is correct
      (test (eq (module function) *verifying-module*)
            "Function ~a is in the wrong module" function)
      ;; Reachability etc
      (let ((reachable (cleavir-set:empty-set)))
        (flet ((iblock-verifier (iblock)
                 (verify iblock)
                 ;; A function has at most one return instruction
                 (test (if (eq iblock end)
                           t
                           (not (typep (end iblock) 'returni)))
                       "iblock ~a is not the end, but ends in a returni"
                       iblock)
                 (cleavir-set:nadjoinf reachable iblock)))
          (map-reachable-iblocks #'iblock-verifier start)
          ;; All reachable blocks are in the iblocks set
          (test (cleavir-set:set<= reachable (iblocks function))
                "Some reachable iblocks ~a are not recorded by the function ~a"
                (cleavir-set:difference 'list reachable (iblocks function))
                function)
          ;; All members of the iblocks set are reachable
          (test (cleavir-set:set<= (iblocks function) reachable)
                "Some iblocks recorded by the function ~a are unreachable: ~a"
                function
                (cleavir-set:difference 'list (iblocks function) reachable)))
        ;; The end block, if it exists, is reachable and in the iblocks set.
        (let ((end (end function)))
          (when end
            (test (cleavir-set:presentp end reachable)
                  "The end block of function ~a is not reachable."
                  function)
            (test (cleavir-set:presentp end (iblocks function))
                  "The end block of function ~a is not recorded."
                  function)))))))

(defmethod verify progn ((module module))
  (let ((*seen-instructions* (cleavir-set:empty-set))
        (*seen-lists* (cleavir-set:empty-set))
        (*seen-next* (cleavir-set:empty-set))
        (*verifying-module* module))
    (cleavir-set:mapset nil #'verify (functions module))))
