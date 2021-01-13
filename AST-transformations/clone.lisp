(cl:in-package #:cleavir-ast-transformations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cloning an AST.

(defun clone-ast (ast)
  (let ((clones (make-hash-table :test #'eq)))
    ;; First, mark all descendants of this AST.
    (labels ((mark-descendants (ast)
               (unless (gethash ast clones)
                 (setf (gethash ast clones) nil)
                 (dolist (child (cleavir-ast:children ast))
                   (mark-descendants child)))))
      (mark-descendants ast))
    ;; Then clone all descendants.
    (labels ((clone (node)
               (typecase node
                 ;; Clone the AST and its substructure.
                 (cleavir-ast:ast
                  (multiple-value-bind (clone descendantp)
                      (gethash node clones)
                    (if descendantp
                        (or clone
                            (setf (gethash node clones)
                                  (apply #'make-instance
                                         (class-of node)
                                         (loop for (keyword reader) in (cleavir-io:save-info node)
	                                       collect keyword
	                                       collect (clone (funcall reader node))))))
                        ;; If the AST is not part of the AST being
                        ;; cloned, e.g. because it's an outer block,
                        ;; use the original.
                        node)))
                 ;; If the substructure of an AST is a CONS (typically
                 ;; a proper list), then we obtain the corresponding
                 ;; substructure of the new AST by copying the CONS
                 ;; and cloning the CAR and the CDR.
                 (cons (cons (clone (car node))
	                     (clone (cdr node))))
                 ;; For most objects, such as numbers, symbols,
                 ;; strings, etc, the object to be used in the new AST
                 ;; is the same as the one in the original AST.
                 (t node))))
      (clone ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generating code for cloning an AST.

;;; In step one, we create a dictionary mapping every node in the
;;; original AST to a symbol that will be used as a local variable in
;;; the generated code.  That variable will hold the cloned AST.
(defun codegen-clone-create-dictionary (ast)
  (let ((dictionary (make-hash-table :test #'eq)))
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (node)
       (setf (gethash node dictionary) (gensym)))
     ast)
    dictionary))

(defgeneric codegen-finalize-substructure (object dictionary))

(defmethod codegen-finalize-substructure (object dictionary)
  (declare (ignore dictionary))
  `',object)

(defmethod codegen-finalize-substructure ((object cons) dictionary)
  `(cons ,(codegen-finalize-substructure (car object) dictionary)
	 ,(codegen-finalize-substructure (cdr object) dictionary)))

(defmethod codegen-finalize-substructure ((object cleavir-ast:ast) dictionary)
  (multiple-value-bind (ast present-p)
      (gethash object dictionary)
    (if present-p
	ast
	object)))

(defun codegen-finalize (model dictionary)
  `(reinitialize-instance
    ,(gethash  model dictionary)
    ,@(loop for (keyword reader) in (cleavir-io:save-info model)
	    for value = (funcall reader model)
	    collect keyword
	    collect (codegen-finalize-substructure value dictionary))))

(defun codegen-clone-ast (ast)
  (let ((dictionary (codegen-clone-create-dictionary ast)))
    `(let ,(loop for key being each hash-key of dictionary
		   using (hash-value variable)
		 for class = (class-of key)
		 for class-name = (class-name class)
		 collect `(,variable (make-instance ',class-name)))
       ,@(loop for key being each hash-key of dictionary
		 using (hash-value variable)
	       collect (codegen-finalize key dictionary))
       ,(gethash ast dictionary))))
