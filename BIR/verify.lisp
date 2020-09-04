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

;;; The function currently being verified.
(defvar *verifying-function*)
;;; " iblock
(defvar *verifying-iblock*)

(defmethod verify progn ((value value))
  ;; rtype is an rtype (verify type decl)
  (assert (typep (rtype value) 'rtype)))

(defmethod verify progn ((instruction instruction))
  ;; verify type decls
  (assert (typep (predecessor instruction) '(or instruction null)))
  (assert (typep (successor instruction) '(or instruction null)))
  ;; All inputs are values, and if they're instructions, they dominate this
  ;; instruction (but see KLUDGE above)
  (flet ((validp (v)
           (etypecase v
             (computation (presentp v *seen-instructions*))
             (value t))))
    (assert (every #'validp (inputs instruction))
            ()
            "Instruction ~a, with inputs ~a,
has use-before-define on inputs ~a!"
            instruction (inputs instruction)
            (remove-if #'validp (inputs instruction)))))

(defmethod verify progn ((instruction no-input-mixin))
  ;; No inputs (verify type decl)
  (assert (null (inputs instruction))))

(defmethod verify progn ((instruction one-input-mixin))
  ;; verify type decl
  (assert (and (listp (inputs instruction))
               (= (length (list (inputs instruction))) 1))))

(defun match-jump-types (inputs dests)
  ;; Ensure the number and rtypes of the inputs match those of the dests
  (loop for dest in dests
        for di = (inputs dest)
        do (assert (= (length inputs) (length di)))
           (assert (every (lambda (x y) (rtype= (rtype x) (rtype y)))
                          inputs di))))

(defmethod verify progn ((instruction terminator))
  ;; No successor (verify type decl)
  (assert (null (successor instruction)))
  ;; NEXT is a list of iblocks
  (assert (every (lambda (b) (typep b 'iblock)) (next instruction))))

(defmethod verify progn ((instruction terminator0))
  ;; No NEXT (verify type decl)
  (assert (null (next instruction))))

(defmethod verify progn ((instruction terminator1))
  ;; verify type decl
  (assert (= (length (next instruction)) 1)))

(defmethod verify progn ((inst enclose))
  ;; verify type decls
  (assert (typep (code inst) 'function))
  (assert (or (not (slot-boundp inst '%initializer))
              (typep (initializer inst) 'initialize-closure)))
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
  (assert (typep (unwinds c) 'set))
  (assert (rtype= (rtype c) :continuation))
  ;; check that all unwinds are unwinds
  (assert (set-every (lambda (u) (typep u 'unwind)) (unwinds c)))
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
  (assert (presentp u (unwinds (catch u))))
  ;; ensure there is at least one input (the continuation)
  (assert (> (length (inputs u)) 0))
  ;; ensure the first input is a continuation
  (assert (rtype= (rtype (first (inputs u))) :continuation))
  ;; ensure inputs match destination
  (match-jump-types (rest (inputs u)) (list (destination u))))

(defmethod verify progn ((j local-unwind))
  ;; Make sure there's only one next
  (match-jump-types (inputs j) (next j)))

(defmethod verify progn ((j jump))
  ;; Make sure there's only one next
  (match-jump-types (inputs j) (next j)))

(defmethod verify progn ((eqi eqi))
  ;; Verify next count
  (assert (= (length (next eqi)) 2)))

(defmethod verify progn ((ftm fixed-to-multiple))
  (assert (aggregatep (rtype (first (inputs ftm)))))
  ;; verify type decl
  (assert (rtype= (rtype ftm) :multiple-values)))

(defmethod verify progn ((mtf multiple-to-fixed))
  (assert (rtype= (rtype (first (inputs mtf))) :multiple-values))
  (assert (aggregatep (rtype mtf))))

(defmethod verify progn ((ext extract))
  (assert (aggregatep (rtype (first (inputs ext))))))

(defmethod verify progn ((c create))
  (let ((inputs (inputs c))
        (rt (rtype c)))
    (assert (aggregatep rt))
    (assert (= (aggregate-length rt) (length inputs)))
    (assert (loop for in in inputs
                  for i from 0
                  always (rtype= (rtype in) (aggregate-elt rt i)))
            ()
            "RTYPES ~a of inputs to a CREATE don't match the output rtype ~a"
            (mapcar #'rtype inputs) rt)))

(defmethod verify progn ((iblock iblock))
  ;; All predecessors truly have this as a successor
  (assert (set-every (lambda (p) (member iblock (next (end p)) :test #'eq))
                     (predecessors iblock)))
  ;; All successors have this as a predecessor
  (assert (every (lambda (n) (presentp iblock (predecessors n)))
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
              (presentp (dynamic-environment iblock) *seen-instructions*)))
  ;; Verify each instruction
  (let ((*verifying-iblock* iblock))
    (map-iblock-instructions
     (lambda (i)
       (verify i)
       (setf *seen-instructions* (nset-adjoin i *seen-instructions*)))
     (start iblock))))

(defmethod verify progn ((function function))
  ;; All inputs are arguments
  (assert (every (lambda (a) (typep a 'argument)) (inputs function)))
  (let ((start (start function))
        (end (end function))
        (*seen-instructions* (empty-set))
        (*verifying-function* function))
    ;; start is an iblock (verify type decl)
    (assert (typep start 'iblock))
    ;; end is an iblock (verify type decl)
    (assert (typep end 'iblock))
    ;; End of the end block is a return instruction
    ;; (NOTE: If the end block can be made into a weak reference
    ;;  this would obviously have to change a bit)
    (assert (and (slot-boundp end '%end) (typep (end end) 'returni)))
    (let ((reachable (empty-set)))
      (flet ((iblock-verifier (iblock)
               (verify iblock)
               ;; A function has only one return instruction
               (assert (if (eq iblock end)
                           t
                           (not (typep (end iblock) 'returni))))
               (setf reachable (nset-adjoin iblock reachable))))
        (map-reachable-iblocks #'iblock-verifier start)
        ;; All reachable blocks are in the iblocks set
        #+(or)
        (assert (set= reachable (iblocks function)))))))
