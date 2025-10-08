(defpackage #:maclina->bir
  (:use #:cl)
  (:local-nicknames (#:m #:maclina.machine)
                    (#:mi #:maclina.introspect)
                    (#:bir #:cleavir-bir)
                    (#:set #:cleavir-set)
                    (#:ctype #:cleavir-ctype)
                    #+(or)(#:env #:cleavir-env)
                    (#:policy #:cleavir-compilation-policy)
                    (#:build #:cleavir-bir-builder))
  (:export #:compile-module-into)
  ;; Only defined if the cmp system is loaded
  (:export #:compile-cmodule-into))

(in-package #:maclina->bir)

(defvar *client*)

;;; a "bcfun" is generically a bytecode function that we're compiling.
;;; The genericity is because we sometimes want to compile actual,
;;; already-linked bytecode functions, and sometimes
;;; Maclina compiler functions (see compile.lisp).

(defgeneric locals-size (function)
  (:method ((function m:function))
    (m:locals-frame-size function)))
(defgeneric nvars (function)
  (:method ((function m:function))
    (m:environment-size function)))
(defgeneric fname (function)
  (:method ((function m:function))
    (m:name function)))
(defgeneric lambda-list (function)
  (:method ((function m:function))
    (m:lambda-list function)))

(defgeneric fcell/name (function-cell))
(defgeneric vcell/name (variable-cell))

(defgeneric compile-instruction (mnemonic context &rest args))
(defgeneric start-annotation (annotation context))
(defgeneric end-annotation (annotation context))

;; Do nothing if the instruction is unreachable. Also bind policy & origin.
(defmethod compile-instruction :around (mnemonic context &rest args)
  (declare (ignore mnemonic args))
  (when (reachablep context)
    (let ((bir:*policy* (policy context))
          (bir:*origin* (first (origin-stack context))))
      (call-next-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Useful structure
;;;

;;; Mapping from bytecode functions to IR functions.
(defclass funmap ()
  (;; List of (bcfun irfun closure); see finfo- accessors below.
   (%map :initform nil :accessor fmap)))

(defun make-funmap () (make-instance 'funmap))

(defun find-bcfun (bcfun funmap)
  (find bcfun (fmap funmap) :key #'finfo-bcfun))
(defun find-irfun (irfun funmap)
  (find irfun (fmap funmap) :key #'finfo-irfun))

(defun finfo-bcfun (finfo) (first finfo))
(defun finfo-irfun (finfo) (second finfo))
(defun finfo-closure (finfo) (third finfo))
(defun (setf finfo-closure) (new finfo) (setf (third finfo) new))

(defun add-function (context bcfun irfun closure)
  (push (list bcfun irfun closure) (fmap (funmap context))))

;;; Mapping from IPs to IR blocks. Used throughout compilation of
;;; a bytecode module due to nonlocal exits.
(defclass blockmap ()
  (;; List of (ip irblock context).
   ;; the context is the context that should be used when compilation
   ;; reaches this point.
   (%map :initform nil :accessor bmap)))

(defun make-blockmap () (make-instance 'blockmap))

(defun find-block (ip irblocks)
  (assoc ip (bmap irblocks)))

(defun binfo-ip (binfo) (first binfo))
(defun binfo-irblock (binfo) (second binfo))
(defun binfo-context (binfo) (third binfo))
(defun binfo-receiving (binfo) (fourth binfo))

(defun %add-block (context ip iblock &optional (receiving 0))
  (let ((ncontext (copy-context context)))
    ;; Fix up the context with block inputs
    (if (= receiving -1)
        (setf (mvals ncontext) (first (bir:inputs iblock)))
        (loop with inputs = (bir:inputs iblock)
                initially (assert (= (length inputs) receiving))
              for i in (bir:inputs iblock)
              do (stack-push i ncontext)))
    ;; Record info
    (let ((binfo (list ip iblock ncontext receiving)))
      (push binfo (bmap (blockmap context)))
      ;; and set things up to start the block later.
      (mi:delay ip binfo)))
  iblock)

;;; Set up and return a new iblock for a later IP.
;;; If an iblock has already been set up (e.g. from nested IF), check
;;; for consistency but return the existing block.
;;; NOTE: Alternately we could create a block anyway that just jumps to the
;;; next. This would preserve block names. But who cares about those?
(defun delay-block (context ip
                    &key name (receiving 0)
                      (dynamic-environment
                       (bir:dynamic-environment (inserter context))))
  (let ((einfo (find-block ip (blockmap context))))
    (cond (einfo
           (assert (= receiving (binfo-receiving einfo)))
           (binfo-irblock einfo))
          (t
           (%add-block context ip
                       (make-iblock-r (inserter context) name
                                      :receiving receiving
                                      :dynamic-environment dynamic-environment)
                       receiving)))))

(defun make-bir-function (bytecode-function inserter
                          &optional (module (bir:module inserter)))
  (let* ((lambda-list (lambda-list bytecode-function))
         (function (make-instance 'bir:function
                     :returni nil ; set by :return compilation
                     :name (fname bytecode-function)
                     :lambda-list nil
                     :docstring (documentation bytecode-function t)
                     :original-lambda-list lambda-list
                     ;;:origin (bcfun/spi bytecode-function)
                     :policy nil
                     :attributes nil
                     :module module))
         (start (make-start-block inserter function bytecode-function)))
    (setf (bir:start function) start)
    (set:nadjoinf (bir:functions module) function)
    function))

(defun make-start-block (inserter irfun bcfun)
  (build:make-iblock
   inserter
   :name (symbolicate (write-to-string (fname bcfun))
                      '#:-start)
   :function irfun :dynamic-environment irfun))

;;; A compilation context. Updated for each control point (instruction) in the
;;; bytecode. Contexts corresponding to jump targets are sometimes copied and
;;; used multiple times.
(defclass context ()
  ((%inserter :initform (make-instance 'build:inserter) :reader inserter)
   ;; A list of things representing the bytecode stack at this point.
   ;; Things are _usually_ BIR:DATUMs. They can also be:
   ;; a list of (BIR:DATUM) to indicate that the bytecode has something in a cell
   ;; a list (:multiple-values BIR:DATUM) for push-values etc.
   (%stack :initform nil :initarg :stack :accessor stack)
   ;; An array representing bytecode variables.
   ;; Each entry is a cons (BIR:VARIABLE . cellp),
   ;; or NIL if the bytecode has no corresponding variable at this point.
   (%locals :initarg :locals :accessor locals)
   ;; A BIR:DATUM representing the current multiple-values register.
   ;; It can also be NIL if the register is uninitialized.
   (%mvals :initform nil :initarg :mvals :accessor mvals)
   ;; Is this point reachable (very conservatively)? Bytecode sometimes has
   ;; regions that are obviously unreachable, e.g. after a return-from.
   (%reachablep :initform t :initarg :reachablep :accessor reachablep)
   ;; A stack of normalized optimize specifications.
   ;; entering a bytecode-ast-decls pushes one, exiting pops.
   (%optimize-stack :initarg :optimize-stack :accessor optimize-stack :type list)
   (%policy :initarg :policy :accessor policy)
   (%variable-stack :initform nil :initarg :variable-stack
                    :accessor variable-stack :type list)
   (%origin-stack :initform nil :accessor origin-stack :type list)
   (%module :initarg :module :reader module :type bir:module)
   (%funmap :initarg :funmap :reader funmap :type funmap)
   (%blockmap :initarg :blockmap :reader blockmap :type blockmap)))

(defun make-context (module blockmap funmap)
  (make-instance 'context
    :module module :blockmap blockmap :funmap funmap
    :optimize-stack (list nil) :policy nil))

(defun stack-push (datum context)
  (push datum (stack context)))
(defun stack-pop (context)
  (assert (stack context))
  (pop (stack context)))

(defun context-new-function (context bcfun)
  (setf (stack context) ()
        (locals context) (make-array (locals-size bcfun) :initial-element nil)
        (mvals context) nil
        (reachablep context) t)
  context)

(defun context-new-block (context old-context)
  ;; Basically mutate context to be old-context.
  ;; locals are handled fine by debug-vars annotations.
  (setf (stack context) (stack old-context)
        (mvals context) (mvals old-context)
        (reachablep context) t))

(defun copy-context (context)
  (make-instance 'context
    :stack (copy-list (stack context))
    :mvals (mvals context) :module (module context)
    :blockmap (blockmap context) :funmap (funmap context)
    :reachablep (reachablep context)))

(defun symbolicate (&rest components)
  ;; FIXME: Probably just use concatenate. Or Alexandria directly
  (let* ((strings (mapcar #'string components))
         (length (reduce #'+ strings :key #'length))
         (name (make-array length :element-type 'character)))
    (let ((index 0))
      (dolist (string strings (make-symbol name))
        (replace name string :start1 index)
        (incf index (length string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler overall
;;;

;;; Process a bytecode module's literals into a vector usable below.
;;; Each entry is a cons of the value and an indicator.
;;; The indicator is usually NIL to begin with.
;;; The bytecode-to-BIR process will replace the indicator
;;; with a BIR object. We don't do this up front because we don't know
;;; whether e.g. a literal bytecode function is going to be called
;;; (necessitating a BIR:CONSTANT) or closed over (necessitating a
;;; BIR:FUNCTION) until we see it used.
(defun compute-runtime-literals (literals mutables)
  (loop with nlits = (length literals)
        with result = (make-array nlits)
        for i from 0 below nlits
        for lit = (aref literals i)
        do (setf (aref result i)
                 (if (member i mutables) ; FIXME: a bit inefficient
                     (cons `',lit :ltv-mutable)
                     (cons lit nil)))
        finally (return result)))

;;; Shared entry point for both runtime and compile-file-time.
(defun compile-bytecode-into (client bytecode annotations literals irmodule)
  (let* ((*client* client) (blockmap (make-blockmap)) (funmap (make-funmap))
         (context (make-context irmodule blockmap funmap)))
    ;; Compile.
    (mi:map-annotated-instructions-literals
     #'compile-instruction #'start-annotation #'end-annotation #'start-block
     bytecode literals annotations
     :context context)
    ;; Compute all iblock flow orders.
    (loop for entry in (fmap funmap)
          do (bir:compute-iblock-flow-order (finfo-irfun entry)))
    ;; Delete any delayed iblocks that turned out to be unreachable
    ;; (e.g. the -after of (tagbody loop ... (go loop))).
    ;; We have to do this manually because compute-iblock-flow-order, which
    ;; normally deletes unused blocks, doesn't even know about them
    ;; as they are not accessible by control flow from the rest of the IR.
    (loop for entry in (bmap blockmap)
          do (bir:maybe-delete-iblock (binfo-irblock entry)))
    ;; Return the funmap.
    funmap))

(defun compile-module-into (client module irmodule)
  (compile-bytecode-into client
                         (m:bytecode module) (m:pc-map module)
                         (compute-runtime-literals (m:literals module) ()) ; FIXME
                         irmodule))

(defun start-block (binfo context)
  ;; If we're falling through from an existing block
  ;; compile in an implicit jump.
  (when (and (reachablep context)
             (not (bir:terminatedp (bir:iblock (inserter context)))))
    (%compile-jump context binfo))
  ;; Start new block.
  (build:begin (inserter context) (binfo-irblock binfo))
  (context-new-block context (binfo-context binfo)))

;;; Given a bytecode function, compile it into the given IR module.
;;; the BIR:FUNCTION is returned.
;;; Used in compile-type-decl.
(defun compile-bcfun-into (function irmodule)
  (let ((fmap (compile-module-into *client* (m:module function) irmodule)))
    (finfo-irfun (find-bcfun function fmap))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compilation of individual instruction types
;;;

(defmethod compile-instruction ((mnemonic (eql 'm:ref)) context &rest args)
  (destructuring-bind (varindex) args
    (let* ((varinfo (aref (locals context) varindex))
           (var (car varinfo))
           (cellp (cdr varinfo)))
      (stack-push
       (etypecase var
         (bir:variable
          (if cellp varinfo (read-variable var context)))
         (bir:come-from var)
         (bir:argument var)) ; happens from e.g. encelled parameters.)
       context))))

(defun read-variable (variable context)
  (let (#+(or)(ctype (declared-ctype (bir:name variable) context)))
    (%read-variable variable (inserter context))
    #+(or)
    (compile-type-decl :variable ctype
                       (%read-variable variable inserter)
                       inserter context)))

#+(or)
(defun declared-ctype (name context)
  (loop for (_ . map) in (variable-stack context)
        for pair = (assoc name map)
        when pair return (cdr pair)
        finally (return (ctype:top *client*))))

(defun %read-variable (variable inserter)
  (let ((readvar-out (make-instance 'bir:output :name (bir:name variable))))
    (build:insert inserter 'bir:readvar
                  :inputs (list variable) :outputs (list readvar-out))
    readvar-out))

(defmethod compile-instruction ((mnemonic (eql 'm:const)) context &rest args)
  (destructuring-bind (c) args
    (let ((output (make-instance 'bir:output))
          (value (car c)) (existing (cdr c))
          (inserter (inserter context)))
      (etypecase existing
        (null ; constant, not yet processed
         (let ((const (build:constant inserter value)))
           (setf (cdr c) const)
           (build:insert inserter 'bir:constant-reference
                         :inputs (list const) :outputs (list output))))
        ((eql :ltv-mutable)
         (let ((ltv (bir:load-time-value-in-module value t (bir:module inserter))))
           (setf (cdr c) ltv)
           (build:insert inserter 'bir:load-time-value-reference
                         :inputs (list ltv) :outputs (list output))))
        ((eql :cfunction)
         ;; A cfunction. This will be a non-closure function at runtime,
         ;; but no function exists yet, so we have to make an ENCLOSE
         ;; instruction. (The translator will not put in any consing,
         ;; since again, this isn't a closure.)
         ;; Bytecode functions don't need to go through this, although
         ;; doing so might help inlining and such? TODO
         (let ((irfun (make-bir-function value inserter)))
           (setf (cdr c) irfun)
           (add-function context value irfun nil)
           (build:insert inserter 'bir:enclose
                         :code irfun
                         :outputs (list output))))
        (bir:constant
         (build:insert inserter 'bir:constant-reference
                       :inputs (list existing) :outputs (list output)))
        (bir:load-time-value
         (build:insert inserter 'bir:load-time-value-reference
                       :inputs (list existing) :outputs (list output)))
        #+(or)
        (cmp:cfunction
         ;; should be impossible as cfunctions only appear once,
         ;; but just in case
         (let* ((finfo (find-bcfun existing (funmap context)))
                (irfun (finfo-irfun finfo)))
           (assert irfun)
           (build:insert inserter 'bir:enclose :code irfun
                         :outputs (list output)))))
      (stack-push output context))))

(defun compile-constant (value inserter)
  (let* ((const (build:constant inserter value))
         (cref-out (make-instance 'bir:output)))
    (build:insert inserter 'bir:constant-reference
                  :inputs (list const) :outputs (list cref-out))
    cref-out))

(defmethod compile-instruction ((mnemonic (eql 'm:closure)) context &rest args)
  (destructuring-bind (index) args
    (let* ((ifun (bir:function (inserter context)))
           (lexes (finfo-closure (find-irfun ifun (funmap context))))
           (lex (elt lexes index)))
      (stack-push (etypecase lex
                    ((cons bir:variable (eql t)) lex) ; cell
                    ((cons bir:variable (eql nil))
                     (read-variable (car lex) context))
                    (bir:come-from lex))
                  context))))

(defmethod compile-instruction ((mnemonic (eql 'm:call)) context &rest args)
  (destructuring-bind (nargs) args
    (setf (mvals context) (compile-call nargs context))))

(defmethod compile-instruction ((mnemonic (eql 'm:call-receive-one)) context &rest args)
  (destructuring-bind (nargs) args
    (setf (mvals context) nil) ; invalidate for self-consistency checks
    (stack-push (compile-call nargs context) context)))

(defmethod compile-instruction ((mnemonic (eql 'm:call-receive-fixed)) context &rest args)
  (destructuring-bind (nargs nvals) args
    (assert (zerop nvals)) ; FIXME
    (setf (mvals context) nil) ; invalidate for self-consistency checks
    (compile-call nargs context)))

(defun compile-call (nargs context)
  (let* ((inserter (inserter context))
         (args (gather context nargs))
         (callee (stack-pop context))
         #+(or)
         (ftype (callee-ftype callee inserter))
         (rargs args #+(or)(type-wrap-arguments ftype args inserter context))
         (out (make-instance 'bir:output))
         #+(or)
         (sys *client*))
    (build:insert inserter 'bir:call
                  :inputs (list* callee rargs)
                  :outputs (list out))
    out
    #+(or)
    (compile-type-decl :return (ctype:function-values ftype sys) out
                       inserter context)))

(defun gather (context n)
  (loop with args = nil
        repeat n
        do (push (stack-pop context) args)
        finally (return args)))

;;; Get the ftype of an output.
;;; Right now this is trivial, but - TODO - might be more involved when
;;; we support local ftype declarations.
#+(or)
(defun callee-ftype (callee context)
  (declare (ignore context))
  (let* ((sys *client*)
         ;; We use the asserted type rather than the derived because
         ;; that's what :fdefinition etc put in, and also because we've yet to
         ;; actually derive anything.
         (vct (bir:asserted-type callee))
         (ct (ctype:primary vct sys)))
    (if (ctype:functionp ct sys)
        ct
        (ctype:function-top sys))))

;;; Given an ftype and a list of argument outputs, return a new list of argument
;;; outputs that are type wrapped if that's useful.
;;; Also warn on argument count mismatch.
#+(or)
(defun type-wrap-arguments (ftype args inserter context)
  (let* ((sys *client*)
         (req (ctype:function-required ftype sys))
         (opt (ctype:function-optional ftype sys))
         (rest (ctype:function-rest ftype sys))
         (rrest (cond ((not (ctype:bottom-p rest sys)) rest)
                      ;; Arguably FIXME that we can have &rest nil with keys.
                      ((ctype:function-keysp ftype sys) (ctype:top sys))
                      (t rest)))
         (min (length req))
         (max (if (not (ctype:bottom-p rrest sys))
                  nil
                  (+ min (length opt))))
         (nargs (length args)))
    (when (or (< nargs min) (and max (> nargs max)))
      (warn 'cmp:wrong-argcount-warning
            :given-nargs nargs :min-nargs min :max-nargs max
            :origin (cst:source bir:*origin*))
      (return-from type-wrap-arguments args))
    (loop for arg in args
          for ctype = (cond (req (pop req))
                            (opt (pop opt))
                            (t rrest))
          collect (compile-type-decl :argument ctype arg inserter context))))

(defmethod compile-instruction ((mnemonic (eql 'm:bind)) context &rest args)
  (destructuring-bind (nvars base) args
    (loop with locals = (locals context)
          for i from (+ base nvars -1) downto base
          for value = (stack-pop context)
          do (set-local locals i value context))))

(defun set-local (locals index value context)
  (let ((local (aref locals index)))
    (if (null local)
        (setf (aref locals index) (cons value nil)) ; FIXME cell
        (let ((var (car local)))
          (if (typep var 'bir:variable)
              (write-variable var value context)
              ;; happens when bindings are closed over.
              (setf (aref locals index)
                    (etypecase value
                      (bir:linear-datum (cons value nil))
                      ((cons bir:linear-datum)
                       (cons (car value) t)))))))))

(defmethod compile-instruction ((mnemonic (eql 'm:set)) context &rest args)
  (destructuring-bind (index) args
    (set-local (locals context) index (stack-pop context) context)))

(defun %write-variable (variable value inserter)
  (assert (slot-boundp variable 'bir::%binder))
  (build:insert inserter 'bir:writevar
                :inputs (list value) :outputs (list variable)))

(defun write-variable (variable value context)
  (let (#+(or)(ctype (declared-ctype (bir:name variable) context)))
    (%write-variable variable value
                     #+(or)(compile-type-decl :setq ctype value inserter context)
                     (inserter context))))

(defmethod compile-instruction ((mnemonic (eql 'm:make-cell)) context &rest args)
  (destructuring-bind () args
    (let ((top (stack-pop context)))
      (check-type top bir:linear-datum)
      ;; Mark as a cell.
      (stack-push (list top) context))))

(defmethod compile-instruction ((mnemonic (eql 'm:encell)) context &rest args)
  (destructuring-bind (varindex) args
    (let* ((varinfo (aref (locals context) varindex))
           (arg (car varinfo)) (cellp (cdr varinfo)))
      ;; encell is only generated for arguments; ergo
      (check-type arg bir:argument)
      (assert (not cellp))
      (setf (aref (locals context) varindex) (cons arg t)))))

(defmethod compile-instruction ((mnemonic (eql 'm:cell-ref)) context &rest args)
  (destructuring-bind () args
    (let ((cell (stack-pop context)))
      ;; Make sure it's a bound cell
      ;; (not a linear datum directly from make-cell; that's illegal bytecode)
      (check-type cell (cons bir:variable))
      (stack-push (read-variable (car cell) context) context))))

(defmethod compile-instruction ((mnemonic (eql 'm:cell-set)) context &rest args)
  (destructuring-bind () args
    (let ((cell (stack-pop context)) (val (stack-pop context)))
      (check-type cell (cons bir:variable))
      (check-type val bir:linear-datum)
      (write-variable (car cell) val context))))

;;; used by make-closure and protect instructions.
(defun make-closure (template context)
  (let* ((inserter (inserter context))
         (irfun (make-bir-function template inserter))
         (enclose-out (make-instance 'bir:output
                        :name (fname template)))
         (nclosed (nvars template))
         (closed (nreverse (subseq (stack context) 0 nclosed)))
         (real-closed (mapcar #'resolve-closed closed)))
    (assert (every (lambda (v) (typep v '(or bir:come-from
                                          (cons bir:variable))))
                   real-closed))
    (build:insert inserter 'bir:enclose
                  :code irfun :outputs (list enclose-out))
    (add-function context template irfun real-closed)
    (setf (stack context) (nthcdr nclosed (stack context)))
    (values enclose-out irfun)))

(defmethod compile-instruction ((mnemonic (eql 'm:make-closure)) context &rest args)
  (destructuring-bind (const) args
    (destructuring-bind (template . existing) const
      ;; any given function is only closed over in one place.
      (assert (member existing '(nil :cfunction)))
      (multiple-value-bind (closure irfun) (make-closure template context)
        (setf (cdr const) irfun)
        (stack-push closure context)))))

(defmethod compile-instruction ((mnemonic (eql 'm:make-uninitialized-closure))
                                context &rest args)
  ;; Set up an ir function for the funmap and generate an enclose,
  ;; but leave the closure for initialize-closure.
  (destructuring-bind (const) args
    (destructuring-bind (template . existing) const
      (assert (member existing '(nil :cfunction)))
      (let* ((inserter (inserter context))
             (irfun (make-bir-function template inserter))
             (enclose-out (make-instance 'bir:output
                            :name (fname template))))
        (setf (cdr const) irfun)
        (build:insert inserter 'bir:enclose
                      :code irfun :outputs (list enclose-out))
        (add-function context template irfun nil)
        (stack-push enclose-out context)))))

(defmethod compile-instruction ((mnemonic (eql 'm:initialize-closure))
                                context &rest args)
  ;; The function has already been put in the funmap and enclosed,
  ;; so just set up its closure before it's generated.
  (destructuring-bind (index) args
    (destructuring-bind (var . cellp) (aref (locals context) index)
      (check-type var bir:variable) (assert (not cellp))
      (let* ((enclose (bir:definition (bir:input (bir:binder var))))
             (finfo (find-irfun (bir:code enclose) (funmap context)))
             (template (finfo-bcfun finfo))
             (nclosed (nvars template))
             (closed (nreverse (subseq (stack context) 0 nclosed)))
             (real-closed (mapcar #'resolve-closed closed)))
        (setf (stack context) (nthcdr nclosed (stack context))
              (finfo-closure finfo) real-closed)))))

(defun resolve-closed (v)
  ;; Each thing on the stack is either a come-from, a list
  ;; (variable . t) if there's a cell, or the output of a readvar for
  ;; variables with no cell.
  (etypecase v
    (bir:come-from v)
    (bir:output (cons (variable-from-output v) nil))
    ((cons bir:variable (eql t)) v)))

(defun variable-from-output (output)
  (let ((def (bir:definition output)))
    (etypecase def
      (bir:readvar (bir:input def))
      (bir:thei
       (let ((tdef (bir:definition (bir:input def))))
         (check-type tdef bir:readvar)
         (bir:input tdef))))))

(defmethod compile-instruction ((mnemonic (eql 'm:return)) context &rest args)
  (destructuring-bind () args
    ;; KLUDGE
    (unless (bir:terminatedp (bir:iblock (inserter context)))
      (let ((input (mvals context)))
        (check-type input bir:linear-datum)
        (build:terminate (inserter context) 'bir:returni :inputs (list input))))))

(defmethod compile-instruction ((mnemonic (eql 'm:bind-required-args))
                                context &rest args)
  (destructuring-bind (nreq) args
    (loop with ifun = (bir:function (inserter context))
          with locals = (locals context)
          for i from 0 below nreq
          for arg = (make-instance 'bir:argument :function ifun)
          do (setf (aref locals i) (cons arg nil))
          collect arg into args
          finally (setf (bir:lambda-list ifun) args))))

(defmethod compile-instruction ((mnemonic (eql 'm:bind-optional-args))
                                context &rest args)
  (destructuring-bind (start nopt) args
    (declare (ignore start))
    (let* ((ifun (bir:function (inserter context)))
           (ll (bir:lambda-list ifun)))
      (loop with ll-app
            for arg = (make-instance 'bir:argument :function ifun)
            for -p = (make-instance 'bir:argument :function ifun)
            repeat nopt
            do (stack-push arg context)
               (push (list arg -p) ll-app)
            finally (setf (bir:lambda-list ifun)
                          (append ll '(&optional) ll-app))))))

(defmethod compile-instruction ((mnemonic (eql 'm:listify-rest-args))
                                context &rest args)
  (destructuring-bind (start) args
    (declare (ignore start))
    (let* ((ifun (bir:function (inserter context)))
           (ll (bir:lambda-list ifun))
           (rarg (make-instance 'bir:argument :function ifun)))
      (setf (bir:lambda-list ifun) (append ll `(&rest ,rarg)))
      (stack-push rarg context))))

(defgeneric constant-value (constant)
  (:method ((c symbol)) c))

(defmethod compile-instruction ((mnemonic (eql 'm:parse-key-args)) context &rest args)
  (destructuring-bind (start (key-count . aokp) keys) args
    (declare (ignore start key-count))
    (let* ((ifun (bir:function (inserter context)))
           (ll (bir:lambda-list ifun)))
      ;; The keys are put on the stack such that
      ;; the leftmost key is the _last_ pushed, etc.
      (loop for ckey in keys
            for key = (constant-value ckey)
            for arg = (make-instance 'bir:argument :function ifun)
            for -p = (make-instance 'bir:argument :function ifun)
            collect (list key arg -p) into ll-app
            finally (setf (bir:lambda-list ifun)
                          (append ll '(&key) ll-app
                                  (if aokp '(&allow-other-keys) ())))
                    (loop for (_1 arg _2) in (nreverse ll-app)
                          do (stack-push arg context))))))

(defun compile-jump (context destination)
  ;; The destination must have already been put in.
  (let ((binfo (find-block destination (blockmap context))))
    (assert binfo)
    (%compile-jump context binfo))
  (setf (reachablep context) nil))

(defun %compile-jump (context binfo)
  (let* ((irblock (binfo-irblock binfo))
         (receiving (binfo-receiving binfo))
         (inputs (if (eql receiving -1)
                     (list (mvals context))
                     (loop with result
                           repeat receiving
                           do (push (stack-pop context) result)
                           finally (return result))))
         (outputs (copy-list (bir:inputs irblock))))
    (assert (loop for i in inputs always (typep i 'bir:linear-datum)))
    (build:terminate (inserter context) 'bir:jump
                     :next (list irblock)
                     :inputs inputs :outputs outputs)))

(defmethod compile-instruction ((mnemonic (eql 'm:jump-8)) context &rest args)
  (apply #'compile-jump context args))
(defmethod compile-instruction ((mnemonic (eql 'm:jump-16)) context &rest args)
  (apply #'compile-jump context args))
(defmethod compile-instruction ((mnemonic (eql 'm:jump-24)) context &rest args)
  (apply #'compile-jump context args))

(defun compile-jump-if (context destination)
  (let* ((inserter (inserter context))
         (condition (stack-pop context))
         (then-dest (delay-block context destination :name '#:if-then))
         (else-dest (build:make-iblock
                     inserter :name '#:if-else)))
    (build:terminate inserter 'bir:ifi
                     :inputs (list condition)
                     :next (list then-dest else-dest))
    ;; The else block we just start here.
    (build:begin inserter else-dest)))

(defmethod compile-instruction ((mnemonic (eql 'm:jump-if-8)) context &rest args)
  (apply #'compile-jump-if context args))
(defmethod compile-instruction ((mnemonic (eql 'm:jump-if-16)) context &rest args)
  (apply #'compile-jump-if context args))
(defmethod compile-instruction ((mnemonic (eql 'm:jump-if-24)) context &rest args)
  (apply #'compile-jump-if context args))

(defun compile-jump-if-supplied (context true-dest)
  (let ((arg (stack-pop context)))
    (check-type arg bir:argument) ; from bind-optional-args
    ;; Now find the corresponding -p argument and branch on it.
    (let* ((inserter (inserter context))
           (ifun (bir:function inserter))
           (-p (loop for e in (bir:lambda-list ifun)
                     when (consp e)
                       do (ecase (length e)
                            (2 (when (eq (first e) arg)
                                 (return (second e))))
                            (3 (when (eq (second e) arg)
                                 (return (third e)))))))
           (thenb (build:make-iblock inserter :name '#:if-supplied))
           (elseb (build:make-iblock inserter :name '#:if-unsupplied))
           (endb (delay-block context true-dest
                              :name '#:if-supplied
                              :receiving 1)))
      (build:terminate inserter 'bir:ifi
                       :inputs (list -p)
                       :next (list thenb elseb))
      (build:begin inserter thenb)
      ;; This block does nothing but jump immediately to the end,
      ;; sending the arg.
      (build:terminate inserter 'bir:jump
                       :inputs (list arg) :outputs (bir:inputs endb)
                       :next (list endb))
      (build:begin inserter elseb))))

(defmethod compile-instruction ((mnemonic (eql 'm:jump-if-supplied-8))
                                context &rest args)
  (apply #'compile-jump-if-supplied context args))
(defmethod compile-instruction ((mnemonic (eql 'm:jump-if-supplied-16))
                                context &rest args)
  (apply #'compile-jump-if-supplied context args))

(defmethod compile-instruction ((mnemonic (eql 'm:check-arg-count-<=))
                                context &rest args)
  (declare (ignore context args)))
(defmethod compile-instruction ((mnemonic (eql 'm:check-arg-count->=))
                                context &rest args)
  (declare (ignore context args)))
(defmethod compile-instruction ((mnemonic (eql 'm:check-arg-count-=))
                                context &rest args)
  (declare (ignore context args)))

(defmethod compile-instruction ((mnemonic (eql 'm:push-values)) context &rest args)
  (destructuring-bind () args
    (let* ((inserter (inserter context))
           (mv (mvals context))
           (during (build:make-iblock inserter :name '#:save-values))
           (save-out (make-instance 'bir:output :name '#:saved-values)))
      (build:terminate inserter 'bir:values-save
                       :inputs (list mv) :outputs (list save-out)
                       :next (list during))
      (setf (mvals context) nil)
      (stack-push (list :multiple-values save-out) context)
      (build:begin inserter during))))

(defmethod compile-instruction ((mnemonic (eql 'm:append-values)) context &rest args)
  (destructuring-bind () args
    (let* ((inserter (inserter context))
           (mv (mvals context))
           (during (build:make-iblock inserter :name '#:save-values))
           (save-out (make-instance 'bir:output :name '#:saved-values))
           (previous (stack-pop context)))
      (check-type previous (cons (eql :multiple-values)))
      (build:terminate inserter 'bir:values-save
                       :inputs (list mv) :outputs (list save-out)
                       :next (list during))
      (setf (mvals context) nil)
      (stack-push (list* :multiple-values save-out (cdr previous)) context)
      (build:begin inserter during))))

(defmethod compile-instruction ((mnemonic (eql 'm:pop-values)) context &rest args)
  (destructuring-bind () args
    (let ((previous (stack-pop context)))
      (check-type previous (cons (eql :multiple-values)
                                 (cons bir:linear-datum null)))
      (let* ((inserter (inserter context))
             (mv (second previous))
             (save (bir:definition mv))
             (read-out (make-instance 'bir:output
                         :name '#:restored-values))
             (old-de (bir:dynamic-environment save))
             (after (build:make-iblock inserter
                                       :name '#:mv-prog1-after
                                       :dynamic-environment old-de)))
        (check-type save bir:values-save)
        (setf (mvals context) read-out)
        (build:insert inserter 'bir:values-restore
                      :inputs (list mv) :outputs (list read-out))
        (build:terminate inserter 'bir:jump
                         :inputs () :outputs () :next (list after))
        (build:begin inserter after)))))

(defmethod compile-instruction ((mnemonic (eql 'm:mv-call)) context &rest args)
  (destructuring-bind () args
    (setf (mvals context) (compile-mv-call context))))
(defmethod compile-instruction ((mnemonic (eql 'm:mv-call-receive-one))
                                context &rest args)
  (destructuring-bind () args
    (stack-push (compile-mv-call context) context)
    (setf (mvals context) nil)))
(defmethod compile-instruction ((mnemonic (eql 'm:mv-call-receive-fixed))
                                context &rest args)
  (destructuring-bind (nvals) args
    (assert (zerop nvals)) ; FIXME
    (compile-mv-call context)
    (setf (mvals context) nil)))

(defun compile-mv-call (context)
  (let ((inserter (inserter context))
        (previous (stack-pop context))
        (callee (stack-pop context))
        (out (make-instance 'bir:output)))
    (check-type previous (cons (eql :multiple-values) cons))
    (let* ((last-arg (second previous))
           (lastdef (bir:definition last-arg))
           (mv (bir:output lastdef))
           (args (reverse (rest previous)))
           (firstdef (bir:definition (first args)))
           (old-de (bir:dynamic-environment firstdef))
           (after (build:make-iblock inserter :name '#:mv-call-after
                                              :dynamic-environment old-de)))
      (check-type lastdef bir:values-save)
      ;; Morph the most recent values-save into a -collect
      ;; so that we have a proper mv call
      (change-class lastdef 'bir:values-collect
                    :inputs (append (butlast args) (bir:inputs lastdef)))
      ;; Generate the actual call
      (build:insert inserter 'bir:mv-call
                    :inputs (list callee mv) :outputs (list out))
      (build:terminate inserter 'bir:jump
                       :inputs () :outputs () :next (list after))
      (build:begin inserter after))
    out))

(defmethod compile-instruction ((mnemonic (eql 'm:save-sp)) context &rest args)
  (destructuring-bind (index) args
    (setf (aref (locals context) index) (cons (stack context) nil))))

(defmethod compile-instruction ((mnemonic (eql 'm:restore-sp)) context &rest args)
  (destructuring-bind (index) args
    (destructuring-bind (stack . cellp) (aref (locals context) index)
      (assert (not cellp)) (assert (listp stack))
      (setf (stack context) stack))))

(defmethod compile-instruction ((mnemonic (eql 'm:entry)) context &rest args)
  (destructuring-bind (index) args
    (let* ((inserter (inserter context))
           (during (build:make-iblock inserter :name '#:block))
           (cf (build:terminate inserter 'bir:come-from
                                :next (list during))))
      (setf (aref (locals context) index) (cons cf nil))
      (build:begin inserter during))))

(defmethod compile-instruction ((mnemonic (eql 'm:exit-8)) context &rest args)
  (destructuring-bind (destination) args
    (compile-exit context destination)))
(defmethod compile-instruction ((mnemonic (eql 'm:exit-16)) context &rest args)
  (destructuring-bind (destination) args
    (compile-exit context destination)))
(defmethod compile-instruction ((mnemonic (eql 'm:exit-24)) context &rest args)
  (destructuring-bind (destination) args
    (compile-exit context destination)))

(defun compile-exit (context destination)
  (let* ((dinfo (find-block destination (blockmap context)))
         (dest (binfo-irblock dinfo))
         (receiving (binfo-receiving dinfo))
         (cf (stack-pop context))
         (inputs (if (= receiving -1)
                     (prog1 (list (mvals context)) (setf (mvals context) nil))
                     (loop repeat receiving collect (stack-pop context)))))
    (check-type cf bir:come-from)
    (build:terminate (inserter context) 'bir:unwind
                     :inputs inputs
                     :outputs (copy-list (bir:inputs dest))
                     :come-from cf
                     :destination dest)
    ;; Don't add duplicate NEXT entries
    ;; (obscure NLX uses can hit this, like CORE::PACKAGES-ITERATOR)
    (unless (eql dest (first (bir:next cf)))
      (pushnew dest (rest (bir:next cf)))
      (set:nadjoinf (bir:predecessors dest) (bir:iblock cf))))
  (setf (reachablep context) nil))

;;; FIXME: The iblocks generated here are often kind of pointless -
;;; i.e. only reachable by unwinding and then all they do is unwind more.
;;; Cleavir could maybe optimize such blocks away.
(defmethod compile-instruction ((mnemonic (eql 'm:entry-close)) context &rest args)
  (destructuring-bind () args
    (let* ((inserter (inserter context))
           (de (bir:parent (bir:dynamic-environment inserter)))
           (ib (build:make-iblock
                inserter :name '#:entry-close :dynamic-environment de)))
      (build:terminate inserter 'bir:jump
                       :inputs () :outputs () :next (list ib))
      (build:begin inserter ib))))

(defmethod compile-instruction ((mnemonic (eql 'm:special-bind)) context &rest args)
  (destructuring-bind (entry) args
    (let* ((inserter (inserter context))
           (vcell (car entry)) (existing (cdr entry))
           (vname (vcell/name vcell))
           (const (or existing (build:vcell inserter vname)))
           (bname (symbolicate '#:bind- vname))
           (next (build:make-iblock inserter :name bname))
           (value (stack-pop context)))
      (setf (cdr entry) const)
      (build:terminate inserter 'bir:constant-bind
                       :inputs (list const value)
                       :next (list next))
      (build:begin inserter next))))

(defmethod compile-instruction ((mnemonic (eql 'm:symbol-value)) context &rest args)
  (destructuring-bind (entry) args
    (let* ((inserter (inserter context))
           (vcell (car entry)) (existing (cdr entry))
           (vname (vcell/name vcell))
           (const (or existing (build:vcell inserter vname)))
           (out (make-instance 'bir:output :name vname)))
      (setf (cdr entry) const)
      (build:insert inserter 'bir:constant-symbol-value
                    :inputs (list const) :outputs (list out))
      (stack-push out context))))

(defmethod compile-instruction ((mnemonic (eql 'm:symbol-value-set)) context &rest args)
  (destructuring-bind (entry) args
    (let* ((inserter (inserter context))
           (vcell (car entry)) (existing (cdr entry))
           (const (or existing
                      (setf (cdr entry)
                            (build:vcell inserter (vcell/name vcell)))))
           (in (stack-pop context)))
      (build:insert inserter 'bir:set-constant-symbol-value
                    :inputs (list const in)))))

(defmethod compile-instruction ((mnemonic (eql 'm:unbind)) context &rest args)
  (destructuring-bind () args
    (let* ((inserter (inserter context))
           (bind (bir:dynamic-environment inserter))
           (vname (bir:variable-name (first (bir:inputs bind))))
           (ib (build:make-iblock
                inserter :name (symbolicate '#:unbind- vname)
                :dynamic-environment (bir:parent bind))))
      (build:terminate inserter 'bir:jump
                       :inputs () :outputs ()
                       :next (list ib))
      (build:begin inserter ib))))

(defmethod compile-instruction ((mnemonic (eql 'm:fdefinition)) context &rest args)
  (destructuring-bind (entry) args
    (let* ((inserter (inserter context))
           (fcell (car entry)) (existing (cdr entry))
           (fname (fcell/name fcell))
           (const (or existing
                      (setf (cdr entry)
                            (build:fcell inserter fname))))
           #+(or)
           (attributes (clasp-cleavir::function-attributes fname))
           #+(OR)
           (ftype (ctype:single-value (clasp-cleavir::global-ftype fname)
                                      *client*))
           (fdef-out (make-instance 'bir:output
                       :name fname))); :asserted-type ftype :attributes attributes)))
      (build:insert inserter 'bir:constant-fdefinition
                    :inputs (list const) :outputs (list fdef-out))
      (stack-push fdef-out context))))

;; Identical to the above, but BIR should maybe have a
;; CONSTANT-CALLED-FDEFINITION for this.
(defmethod compile-instruction ((mnemonic (eql 'm:called-fdefinition))
                                context &rest args)
  (destructuring-bind (entry) args
    (let* ((inserter (inserter context))
           (fcell (car entry)) (existing (cdr entry))
           (fname (fcell/name fcell))
           (const (or existing
                      (setf (cdr entry)
                            (build:fcell inserter fname))))
           #+(or)
           (attributes (clasp-cleavir::function-attributes fname))
           #+(or)
           (ftype (ctype:single-value (clasp-cleavir::global-ftype fname)
                                      *client*))
           (fdef-out (make-instance 'bir:output
                       :name fname))); :asserted-type ftype :attributes attributes)))
      (build:insert inserter 'bir:constant-fdefinition
                    :inputs (list const) :outputs (list fdef-out))
      (stack-push fdef-out context))))

#+(or)
(defmethod compile-instruction ((mnemonic (eql 'm:fdesignator)) context &rest args)
  ;; Just call CORE:COERCE-CALLED-FDESIGNATOR.
  (destructuring-bind (env) args
    (declare (ignore env))
    (let* ((inserter (inserter context))
           (desig (stack-pop context))
           (fname 'core:coerce-called-fdesignator)
           (const (build:fcell inserter fname))
           #+(or)
           (attributes (clasp-cleavir::function-attributes fname))
           (fdef-out (make-instance 'bir:output
                       :name fname)); :attributes attributes))
           (out (make-instance 'bir:output :name '#:callee)))
      (build:insert inserter 'bir:constant-fdefinition
                    :inputs (list const) :outputs (list fdef-out))
      (build:insert inserter 'bir:call
                    :inputs (list fdef-out desig)
                    :outputs (list out))
      (stack-push out context))))

(defmethod compile-instruction ((mnemonic (eql 'm:protect)) context &rest args)
  (destructuring-bind (const) args
    (destructuring-bind (template . existing) const
      (assert (member existing '(nil :cfunction)))
      (multiple-value-bind (cleanup irfun) (make-closure template context)
        (setf (cdr const) irfun)
        (let* ((inserter (inserter context))
               (body (build:make-iblock inserter :name '#:protect)))
          (build:terminate inserter 'bir:unwind-protect
                           :inputs (list cleanup) :next (list body))
          (build:begin inserter body))))))

(defmethod compile-instruction ((mnemonic (eql 'm:cleanup)) context &rest args)
  (destructuring-bind () args
    (let* ((inserter (inserter context))
           (protect (bir:dynamic-environment inserter))
           (ib (build:make-iblock
                inserter :name '#:post-protection
                         :dynamic-environment (bir:parent protect))))
      (build:terminate inserter 'bir:jump
                       :inputs () :outputs ()
                       :next (list ib))
      (build:begin inserter ib))))

(defmethod compile-instruction ((mnemonic (eql 'm:nil)) context &rest args)
  (destructuring-bind () args
    (stack-push (compile-constant 'nil (inserter context)) context)))

(defmethod compile-instruction ((mnemonic (eql 'm:push)) context &rest args)
  (destructuring-bind () args
    (let ((mv (mvals context)))
      (setf (mvals context) nil)
      (stack-push mv context))))

(defmethod compile-instruction ((mnemonic (eql 'm:pop)) context &rest args)
  (destructuring-bind () args
    (let ((mv (stack-pop context)))
      (check-type mv bir:linear-datum)
      (setf (mvals context) mv))))

(defmethod compile-instruction ((mnemonic (eql 'm:dup)) context &rest args)
  (destructuring-bind () args
    (let ((var (make-instance 'bir:variable :ignore nil))
          (inserter (inserter context)))
      (%bind-variable var (stack-pop context) (inserter context))
      (stack-push (%read-variable var inserter) context)
      (stack-push (%read-variable var inserter) context))))

#+(or)
(defun declared-variable-ctype (decls functionp)
  ;; FIXME: Function types will take a little thought, since we want to
  ;; declare types of arguments/return values rather than of the function itself.
  (when functionp (return-from declared-variable-ctype t))
  (loop with env = clasp-cleavir:*clasp-env*
        with sys = *client*
        for decl in decls
        ;; just take the first type decl - FIXME?
        when (and (consp decl) (eq (first decl) 'cl:type))
          return (env:parse-type-specifier (second decl) env sys)
        finally (return (ctype:top sys))))

(defun bind-variable (variable value ctype context)
  #-(or) (declare (ignore ctype))
  (let ((typed value #+(or) (compile-type-decl :setq ctype value context)))
    (%bind-variable variable typed (inserter context))))

(defun %bind-variable (variable value inserter)
  (build:insert inserter 'bir:leti
                :inputs (list value)
                :outputs (list variable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compilation of annotations
;;;

;;; default methods: irrelevant to compilation. ignore.
(defmethod start-annotation ((annot m:map-info) context)
  (declare (ignore context)))
(defmethod end-annotation ((annot m:map-info) context)
  (declare (ignore context)))

(defmethod start-annotation ((annotation m:vars-info) context)
  (when (and (reachablep context)
             (not (degenerate-annotation-p annotation)))
    (loop with bir:*policy* = (policy context)
          for bdv in (m:bindings annotation)
          for name = (m:name bdv)
          for cellp = (m:cellp bdv)
          for index = (m:index bdv)
          for ctype = t
                      #+(or)(declared-variable-ctype
                             (m:declarations bdv) (consp name))
          for (datum) = (aref (locals context) index)
          ;; We make all variables IGNORABLE because the bytecode compiler
          ;; has already warned about any syntactically unused variables
          ;; (and variables declared IGNORE but then used).
          ;; Also, some uses in the original source are not preserved by the
          ;; bytecode compiler, e.g. (progn x nil). So doing ignore stuff
          ;; here results in spurious warnings.
          for variable = (make-instance 'bir:variable
                           :ignore 'cl:ignorable :name name)
          do (etypecase datum
               (bir:linear-datum
                (bind-variable variable datum ctype context))
               ((cons bir:linear-datum) ; cell
                (bind-variable variable (car datum) ctype context)))
             (setf (aref (locals context) index)
                   (cons variable cellp))
          collect (cons name ctype) into typemap
          finally (push (cons annotation typemap) (variable-stack context)))))

(defun degenerate-annotation-p (annotation)
  ;; These can arise naturally from code like
  ;; (progn (let ((y x)) y) more-code)
  ;; or just from THE or something. But for bytecode-debug-vars they pose
  ;; an issue, as we would end the annotation before it begins. So we skip 'em.
  (= (m:start annotation) (m:end annotation)))

(defmethod end-annotation ((annot m:vars-info) context)
  ;; Check if start-annotation actually did anything. If it didn't,
  ;; there's nothing for us to undo.
  (when (eq annot (first (first (variable-stack context))))
    ;; End the extent of all variables.
    (loop for bdv in (m:bindings annot)
          for index = (m:index bdv)
          do (setf (aref (locals context) index) nil))
    ;; And type declarations.
    (pop (variable-stack context))))

(defmethod start-annotation ((annot m:if-info) context)
  (when (reachablep context)
    ;; Record the merge block for later jumps.
    (delay-block context (m:end annot)
                 :name '#:if-merge
                 :receiving (m:receiving annot))))

(defmethod start-annotation ((annot m:tagbody-info) context)
  (when (reachablep context)
    (loop for (name . ip) in (m:tags annot)
          for iblock = (delay-block context ip :name name)
          finally
             ;; Establish a block for after the end.
             (delay-block context (m:end annot)
                          :name '#:tagbody-after))))

(defun make-iblock-r (inserter name
                      &key (receiving 0)
                        (dynamic-environment
                         (bir:dynamic-environment inserter)))
  (build:make-iblock inserter :name name
                              :dynamic-environment dynamic-environment
                              ;; -1 means multiple values means one phi.
                              :ninputs (if (eql receiving -1) 1 receiving)))

(defmethod start-annotation ((annot m:block-info) context)
  (when (reachablep context)
    (let* ((receiving (m:receiving annot))
           (freceiving (if (= receiving 1) -1 receiving))
           (name (m:name annot))
           (end (m:end annot)))
      (delay-block context end
                   :name (symbolicate name '#:-after)
                   :receiving freceiving)
      ;; this and FRECEIVING are to take care of the ugly code we generate
      ;; when a block is in a one-value context. See bytecode_compiler.cc.
      ;; Basically, we have entry -> [body] -> jump normal; exit: push; normal:
      ;; exits jump to the exit label. This is done so that nonlocal return
      ;; values can always be put in the MV vector, but it sure looks ugly.
      (when (= receiving 1)
        (delay-block context (1+ end) ; 1+ for the push.
                     :name (symbolicate name '#:after-push)
                     :receiving receiving)))))

#+(or)
(defmethod start-annotation ((the m:the-info) context)
  (when (reachablep context)
    (let* ((type (m:the-type the))
           (ptype (env:parse-values-type-specifier
                   type clasp-cleavir:*clasp-env* *client*))
           (receiving (m:receiving the)))
      (case receiving
        ((1)
         (stack-push (compile-type-decl :the ptype (stack-pop context) context)
                     context))
        ((-1)
         (setf (mvals context)
               (compile-type-decl :the ptype (mvals context) context)))
        ;; TODO: Something for 0/single values?
        (otherwise)))))

#+(or)
(defun compile-type-decl (which ctype datum context)
  (let ((sys *client*)
        (sv-ctype-p (member which '(:variable :argument :setq))))
    (if (if sv-ctype-p
            (ctype:top-p ctype sys)
            (clasp-cleavir::values-top-p ctype sys))
        datum ; too boring a type annotation to bother with
        (let* ((inserter (inserter context))
               (bir:*policy* (policy context))
               (vctype (ecase which
                         ((:the :return) ctype)
                         ((:variable) (ctype:single-value ctype sys))
                         ((:argument :setq)
                          (ctype:coerce-to-values ctype sys))))
               (out (make-instance 'bir:output
                      :name (bir:name datum)))
               (type-check-function
                 (ecase (clasp-cleavir::insert-type-checks-level bir:*policy* which)
                   ((0) :trusted)
                   ((1) nil)
                   ((2 3) (bytecompile-type-check which ctype sys
                                                  (bir:module inserter))))))
          (build:insert inserter 'bir:thei
                        :inputs (list datum)
                        :outputs (list out)
                        :asserted-type vctype
                        :type-check-function type-check-function)
          out))))

;;; TODO? Probably could cache this, at least for standard types.
#+(or)
(defun bytecompile-type-check (which ctype sys module)
  (compile-bcfun-into
   (cmp:bytecompile (clasp-cleavir::make-type-check-fun which ctype sys))
   module))

;;; FIXME: To get declaration scope right w/ bytecode-debug-vars we'll probably
;;; need to ensure that decls appear before vars in the annotations.
(defmethod start-annotation ((annot m:declarations-info) context)
  (when (degenerate-annotation-p annot)
    (return-from start-annotation))
  (loop with opt = (first (optimize-stack context))
        for (spec . rest) in (m:declarations annot)
        do (case spec
             ((cl:optimize)
              (setf opt
                    (policy:normalize-optimize
                     *client* (append (copy-list rest) opt)))))
        finally (push opt (optimize-stack context))
                (setf (policy context)
                      (policy:compute-policy *client* opt))))
(defmethod end-annotation ((annot m:declarations-info) context)
  (when (degenerate-annotation-p annot)
    (return-from end-annotation))
  (pop (optimize-stack context))
  (setf (policy context) (policy:compute-policy *client*
                                                (first (optimize-stack context)))))

(defmethod start-annotation ((annot m:function) context)
  (begin-function annot context))
(defmethod end-annotation ((annot m:function) context)
  (end-function annot context))

(defun begin-function (function context)
  (let* ((existing (find-bcfun function (funmap context)))
         (inserter (inserter context))
         ;; We might have already made this function, for ENCLOSE.
         (irfun (if existing
                    (finfo-irfun existing)
                    (let ((new
                            (make-bir-function function inserter
                                               (module context))))
                      (push (list function new nil) (fmap (funmap context)))
                      new))))
    (build:begin inserter (bir:start irfun))
    (context-new-function context function))
  #+(or)
  (let ((spi (function-spi function)))
    (when spi
      (push (kludge-spi spi) (origin-stack context)))))

(defun end-function (function context)
  (declare (ignore function context))
  #+(or)
  (when (function-spi annot)
    (pop (origin-stack context))))
