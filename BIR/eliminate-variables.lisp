(in-package #:cleavir-bir)

;;;; Build a function dag.

(defclass dag-node ()
  ((%children :initform (empty-set) :reader children :accessor %children)))

(defgeneric node-function (node))

(defclass function-dag (dag-node)
  (;; The highest level function
   (%top :initarg :top :reader top)
   ;; A hash table from functions to sets of interior-nodes
   ;; such that each interior-node has that function as its CODE
   (%dag-nodes :initarg :dag-ndoes :reader dag-nodes)))

(defmethod node-function ((node dag-node)) (top node))

(defclass interior-node (dag-node)
  ((%parents :initarg :parents :reader parents :type set)
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
            (loop for parent in parents
                  do (nset-adjoinf (%children parent) node))
            (nset-adjoinf (gethash (code instruction) dag-nodes) node)))))
     set)
    root))

;;;; Convert closure variables into cells. (TODO)
;;;; Replace temporary variables, i.e. variables that are only assigned to in
;;;; a single place and are not closed over, with whatever they're assigned to.

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
