(in-package #:cleavir-bir)

(defgeneric verify (bir)
  ;; most-specific-last so that we get the core assertions out of the way;
  ;; failures there may cause hard to understand failures more specifically
  (:method-combination progn :most-specific-last))

;;; KLUDGE: This is somewhat inexact; for example if two blocks share their
;;; only predecessor, one could have a computation in the other as an input,
;;; which is invalid, but if we happened to traverse the blocks in the wrong
;;; order we wouldn't catch it.
(defvar *seen-instructions*)

(defvar *seen-inputs*)
(defvar *seen-next*)

;;; The function currently being verified.
(defvar *verifying-function*)
;;; " iblock
(defvar *verifying-iblock*)

(defmethod verify progn ((datum datum))
  ;; rtype is an rtype (verify type decl)
  (assert (typep (rtype datum) 'rtype)))

(defmethod verify progn ((instruction instruction))
  ;; verify type decls
  (assert (typep (predecessor instruction) '(or instruction null)))
  (assert (typep (successor instruction) '(or instruction null)))
  ;; All inputs are LINEAR-DATUMs, and if they're instructions, they dominate
  ;; this instruction (but see KLUDGE above)
  (flet ((validp (v)
           (etypecase v
             (computation (cleavir-set:presentp v *seen-instructions*))
             (linear-datum t))))
    (assert (every #'validp (inputs instruction))
            ()
            "Instruction ~a, with inputs ~a,
has use-before-define on inputs ~a!"
            instruction (inputs instruction)
            (remove-if #'validp (inputs instruction))))
  ;; Make sure input lists are not shared, so we can destroy them
  (unless (null (inputs instruction))
    (assert (not (cleavir-set:presentp (inputs instruction) *seen-inputs*))
            ;; could track the other instruction sharing it
            () "Inputs list of instruction ~a is shared" instruction)
    (cleavir-set:nadjoinf *seen-inputs* (inputs instruction))))

(defmethod verify progn ((instruction no-input))
  ;; No inputs (verify type decl)
  (assert (null (inputs instruction))))

(defmethod verify progn ((instruction one-input))
  ;; verify type decl
  (assert (and (listp (inputs instruction))
               (= (length (list (inputs instruction))) 1))))

(defmethod verify progn ((instruction no-output))
  ;; verify type decl
  (assert (null (outputs instruction))))

(defun match-jump-types (inputs outputs)
  ;; Ensure the number and rtypes of the inputs match those of the outputs
  (assert (= (length inputs) (length outputs)))
  (assert (every (lambda (x y) (rtype= (rtype x) (rtype y)))
                 inputs outputs)))

(defmethod verify progn ((instruction terminator))
  ;; No successor (verify type decl)
  (assert (null (successor instruction)))
  ;; NEXT is a list of iblocks
  (assert (every (lambda (b) (typep b 'iblock)) (next instruction)))
  ;; NEXT list is not shared and therefore destructible
  (unless (null (next instruction))
    (assert (not (cleavir-set:presentp (next instruction) *seen-next*))
            () "NEXT is shared for instruction: ~a" (next instruction))
    (cleavir-set:nadjoinf *seen-next* (next instruction)))
  ;; Outputs are all PHIs
  (assert (every (lambda (o) (typep o 'phi)) (outputs instruction))))

(defmethod verify progn ((instruction terminator0))
  ;; No NEXT (verify type decl)
  (assert (null (next instruction))))

(defmethod verify progn ((instruction terminator1))
  ;; verify type decl
  (assert (= (length (next instruction)) 1)))

(defmethod verify progn ((inst enclose))
  ;; verify type decls
  (assert (typep (code inst) 'function))
  (verify (code inst)))

(defmethod verify progn ((wv writevar))
  ;; match types
  (assert (rtype= (rtype (first (inputs wv))) (rtype (variable wv)))))

(defmethod verify progn ((rv readvar))
  ;; match types
  (assert (rtype= (rtype rv) (rtype (variable rv)))))

(defmethod verify progn ((call call))
  (assert (> (length (inputs call)) 0))
  ;; verify type decl
  (assert (rtype= (rtype call) :multiple-values)))

(defmethod verify progn ((c catch))
  ;; verify type decls
  (assert (typep (unwinds c) 'cleavir-set:set))
  (assert (rtype= (rtype c) :continuation))
  ;; check that all unwinds are unwinds
  (assert (cleavir-set:every (lambda (u) (typep u 'unwind)) (unwinds c)))
  ;; check that there's at least one next
  (assert (> (length (next c)) 0))
  ;; Check that the normal next has this dynamic environment
  (assert (eq (dynamic-environment (first (next c))) c))
  ;; Check that the abnormal nexts have this block's dynenv
  (loop with de = (dynamic-environment *verifying-iblock*)
        for n in (rest (next c))
        do (assert (eq (dynamic-environment n) de))))

(defmethod verify progn ((u unwind))
  ;; verify type decls
  (assert (typep (catch u) 'catch))
  (assert (typep (destination u) 'iblock))
  ;; Make sure the catch knows about us
  ;; (since if we're being verified, we must be reachable and live)
  (assert (cleavir-set:presentp u (unwinds (catch u))))
  ;; ensure there is at least one input (the continuation)
  (assert (> (length (inputs u)) 0))
  ;; ensure the first input is a continuation
  (assert (rtype= (rtype (first (inputs u))) :continuation))
  ;; ensure inputs match destination
  (match-jump-types (rest (inputs u)) (outputs u)))

(defmethod verify progn ((j jump))
  (match-jump-types (inputs j) (outputs j))
  ;; Check accuracy of unwindp (TODO: Check that the dynenv is a parent)
  ;; (probably that's a general thing for terminators tho)
  (let ((de (dynamic-environment *verifying-iblock*)))
    (assert (or (unwindp j) (eq de (dynamic-environment (first (next j))))))))

(defmethod verify progn ((eqi eqi))
  ;; Verify next count
  (assert (= (length (next eqi)) 2)))

(defmethod verify progn ((ftm fixed-to-multiple))
  ;; verify type decl
  (assert (rtype= (rtype ftm) :multiple-values)))

(defmethod verify progn ((mtf multiple-to-fixed))
  (assert (rtype= (rtype (first (inputs mtf))) :multiple-values)))

(defmethod verify progn ((iblock iblock))
  ;; All predecessors truly have this as a successor
  (assert (cleavir-set:every (lambda (p) (member iblock (next (end p))
                                                     :test #'eq))
                                 (predecessors iblock)))
  ;; All successors have this as a predecessor
  (assert (every (lambda (n) (cleavir-set:presentp iblock (predecessors n)))
                 (next (end iblock))))
  ;; Start is an instruction (verify type decl)
  (assert (typep (start iblock) 'instruction))
  ;; Start instruction has no predecessor
  (assert (null (predecessor (start iblock))))
  ;; End instruction is a terminator (verify type decl)
  (assert (typep (end iblock) 'terminator))
  ;; Dynenv is a dynenv (verify type decl)
  (assert (typep (dynamic-environment iblock) 'dynamic-environment))
  ;; dynenv is either the function itself or an instruction that
  ;; dominates this block (but see KLUDGE above)
  (assert (or (eq (dynamic-environment iblock) *verifying-function*)
              (cleavir-set:presentp
               (dynamic-environment iblock) *seen-instructions*)))
  ;; inputs are all phis
  (assert (every (lambda (i) (typep i 'phi)) (inputs iblock)))
  ;; Verify each instruction
  (let ((*verifying-iblock* iblock))
    (map-iblock-instructions
     (lambda (i)
       (verify i)
       (cleavir-set:nadjoinf *seen-instructions* i))
     (start iblock))))

(defmethod verify progn ((function function))
  (let ((start (start function))
        (end (end function))
        (*seen-instructions* (cleavir-set:empty-set))
        (*verifying-function* function)
        (*seen-inputs* (cleavir-set:empty-set))
        (*seen-next* (cleavir-set:empty-set)))
    ;; start is an iblock (verify type decl)
    (assert (typep start 'iblock))
    ;; end is an iblock (verify type decl)
    (assert (typep end 'iblock))
    ;; End of the end block is a return instruction
    ;; (NOTE: If the end block can be made into a weak reference
    ;;  this would obviously have to change a bit)
    (assert (and (slot-boundp end '%end) (typep (end end) 'returni)))
    (let ((reachable (cleavir-set:empty-set)))
      (flet ((iblock-verifier (iblock)
               (verify iblock)
               ;; A function has only one return instruction
               (assert (if (eq iblock end)
                           t
                           (not (typep (end iblock) 'returni))))
               (cleavir-set:nadjoinf reachable iblock)))
        (map-reachable-iblocks #'iblock-verifier start)
        ;; All reachable blocks are in the iblocks set
        #+(or)
        (assert (cleavir-set:set= reachable (iblocks function)))))))
