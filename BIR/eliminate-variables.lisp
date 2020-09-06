(in-package #:cleavir-bir)

;;;; Build a function dag.

(defclass dag-node ()
  (;; A set of DAG-NODEs; the NODE-FUNCTION of this node owns an enclose
   ;; of every child.
   (%children :initform (empty-set) :reader children :accessor %children)))

(defgeneric node-function (node))

(defclass function-dag (dag-node)
  (;; The highest level function
   (%top :initarg :top :reader top)
   ;; A hash table from functions to sets of interior-nodes
   ;; such that each interior-node has that function as its CODE
   (%dag-nodes :initarg :dag-nodes :reader dag-nodes)))

(defmethod node-function ((node function-dag)) (top node))

(defclass interior-node (dag-node)
  (;; A set of DAG-NODEs; the NODE-FUNCTION of each parent is
   ;; the owner of ENCLOSE.
   (%parents :initarg :parents :reader parents :type set)
   (%enclose :initarg :enclose :reader enclose :type enclose)))

(defmethod node-function ((node dag-node)) (code (enclose node)))

(defun build-function-dag-from-set (top set)
  (check-type top function)
  (check-type set set)
  (let* ((dag-nodes (make-hash-table :test #'eq))
         (root (make-instance 'function-dag
                 :dag-nodes dag-nodes :top top)))
    (setf (gethash top dag-nodes) (make-set root))
    (map-instructions-with-owner-from-set
     (lambda (instruction owner)
       (typecase instruction
         (enclose
          (let* ((parents (gethash owner dag-nodes))
                 (node (make-instance 'interior-node
                         :parents parents :enclose instruction)))
            (doset (parent parents) (nset-adjoinf (%children parent) node))
            (setf (gethash (code instruction) dag-nodes)
                  (nset-adjoin node (gethash (code instruction) dag-nodes
                                             (empty-set))))))))
     set)
    root))

(defun build-function-dag (top)
  (build-function-dag-from-set top (all-functions top)))

;;; Given a function and a DAG, return a set of all functions that enclose
;;; the function, directly or not.
(defun ancestor-functions (function dag)
  (let ((result (empty-set)))
    (labels ((aux (node)
               (when (typep node 'interior-node)
                 (nset-adjoinf result (node-function node))
                 (mapset #'aux (parents node)))))
      (mapset #'aux (gethash function (dag-nodes dag)))
      result)))

;;;; Fill out the OWNER and EXTENT of all variables.

(defun analyze-variables (all-functions dag)
  (doset (funct all-functions (values))
    (let (;; A set of all variables accessed by this function's ancestors.
          (parent-variables
            (let ((pv (empty-set)))
              (mapset (lambda (ancestor)
                        (mapset (lambda (variable)
                                  (nset-adjoinf pv variable))
                                (variables ancestor)))
                      (ancestor-functions funct dag))
              pv)))
      (doset (variable (variables funct))
        (if (presentp variable parent-variables)
            ;; Present in a parent, so it's definitely shared.
            (setf (extent variable) :indefinite)
            (ecase (extent variable)
              ;; Not in a parent, so it's ours. It could be in a child, in
              ;; which case the child will set it to :indefinite above.
              (:unanalyzed
               (setf (owner variable) funct
                     (extent variable) :local))
              ;; Some other function has this variable, but we're not a
              ;; parent of that function and it's not a parent of us.
              ;; This should not be possible. (FIXME: Error message.)
              (:local (error "???"))
              ;; Some other function has already noted this variable is
              ;; indefinite - presumably a child.
              ;; NOTE: We could skip the presentp in this case
              (:indefinite)))))))

(defun closed-over-predicate (function)
  (lambda (variable)
    (and (not (eq (extent variable) :local))
         (not (eq (owner variable) function)))))

(defun mark-enclose-recursively (variables node)
  (when (typep node 'interior-node)
    (let* ((enclose (enclose node))
           (owner (node-function node))
           (parents (parents node))
           (nparents (set-size parents)))
      ;; mark the enclose and function
      (nset-unionf (variables enclose) variables)
      (nset-unionf (variables owner) variables)
      ;; Remove any variables the current function owns
      ;; and while we're at it, update the variables' enclose sets
      (doset (v variables)
        (nset-adjoinf (encloses v) enclose)
        (when (eq (owner v) owner) (nset-removef variables v)))
      (cond ((empty-set-p variables)) ; no more variables: nothing left to do
            ((zerop nparents)) ; at the top: nothing left to do
            ((= nparents 1) ; only one parent, so the set can be destroyed
             (doset (p parents)
               (mark-enclose-recursively variables p)))
            (t ; have to copy the set. (NOTE: We could skip one copy.)
             (doset (p parents)
               (mark-enclose-recursively (copy-set variables) p)))))))

;;; Augment each enclose instruction with the set of variables that need to be
;;; closed over. Augment each function's variable set with any variables that
;;; need to be added for the encloses. Precondition: analyze-variables has run.
(defun transmit-variables (all-functions dag)
  (doset (funct all-functions (values))
    (let ((closed (set-filter (closed-over-predicate funct) (variables funct)))
          (nodes (gethash funct (dag-nodes dag))))
      (if (= (set-size nodes) 1)
          ;; only one node, so we can destroy the set
          (doset (node nodes) (mark-enclose-recursively closed node))
          (doset (node nodes)
            (mark-enclose-recursively (copy-set closed) node))))))

(defun process-captured-variables (ir)
  (let* ((af (all-functions ir))
         (dag (build-function-dag-from-set ir af)))
    (analyze-variables af dag)
    (transmit-variables af dag)))

#+(or)
(defun segregate-lexicals (all-functions)
  (let* (;; An alist of (variable . info)
         ;; where info is a list of (inst . function)
         ;; where each inst is an accessvar instruction for the variable,
         ;; and is owned by the given function.
         (variable-map nil))
    ;; Build the map
    (map-instructions-with-owner-from-set
     (lambda (inst owner)
       (typecase inst
         (accessvar (let* ((variable (variable inst))
                           (new (cons inst owner))
                           (p (assoc variable variable-map)))
                      (if p
                          (push new (cdr p))
                          (push (list variable new) variable-map))))))
     all-functions)
    (loop for entry in variable-map
          for (var . info) = entry
          if (or (< (length info) 2) ; not terribly likely
                 (let ((first-owner (cdar info)))
                   (loop for (enclose . function) in (cdr info)
                         always (eq function first-owner))))
            collect entry into locals
          else collect entry into shared
          finally (return (values locals shared)))))

;;; Given a var and its list of (access . owner), convert to cell manipulation.
;;; Also needs a function-dag.
#+(or)
(defun process-shared (var info function-dag)
  ;; Here's what we do. We analyze each access fairly independently.
  ;; For each access we check the function dag. Any function enclosing the
  ;; function that owns the access either needs to transfer the variable
  ;; through cells (even if it doesn't access the variable itself), or if the
  ;; access is the initial binding, the function that owns the access needs to
  ;; make the cell.
  ;; Honestly this is stupid, we should just record the initial binding in the
  ;; AST
  )

;;; Given a var and its list of accesses, all of which are owned by the same
;;; function, eliminate the variable if possible, i.e. if it has only one
;;; write instruction to its name rewrite all its reads to use whatever is
;;; written to it directly.
;;; Returns true if the variable was eliminated.
#+(or)
(defun process-local (var accesses)
  (let ((writes (remove-if-not (lambda (x) (typep x 'writevar)) accesses)))
    (case (length writes)
      (0 (error "Variable is read but not written: ~a" var))
      (1 ; bingo
       (loop with write = (first writes)
             with val = (first (inputs write))
             for inst in accesses
             if (eq inst write)
               do (delete-instruction inst)
             else do (delete-computation inst val))
       t)
      (otherwise nil)))) ; zonk
