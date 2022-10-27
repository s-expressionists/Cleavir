(cl:in-package #:cleavir-ast-graphviz)

;;;; Drawing an AST. 
;;;;
;;;; We generate a Graphviz source file from the AST so that the AST
;;;; can be presented in graph form for easy inspection.

(defparameter *table* nil)

(defun id (ast)
  (symbol-name (gethash ast *table*)))

(defgeneric stream-draw-ast (ast stream))

(defun draw-ast (ast filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "digraph G {~%   ordering = out;~%")
    (let ((*table* (make-hash-table :test #'eq)))
      (stream-draw-ast ast stream))
    (format stream "}~%")))

(defmethod stream-draw-ast :around (ast stream)
  (when (null (gethash ast *table*))
    (setf (gethash ast *table*) (gensym))
    (format stream "  ~a [shape = box];~%"
            (id ast))
    (call-next-method)
    (loop for child in (children ast)
          for i from 1
          do (stream-draw-ast child stream)
             (format stream "   ~a -> ~a [label = \"~d\"];~%"
                     (id ast) (id child) i))))

(defgeneric label (ast))

;;; The default label is the lower-case version of the name of the
;;; class (as a string) with suffix -ast stripped off. 
(defmethod label (ast)
  (let ((name (string (class-name (class-of ast)))))
    (string-downcase (subseq name 0 (- (length name) 4)))))

(defmacro deflabel (ast label)
  `(defmethod label ((ast ,ast))
     (declare (ignorable ast))
     ,label))

;;; Default method on STREAM-DRAW-AST.  This method simply calls the
;;; generic function LABEL in order to draw a label for the box.
(defmethod stream-draw-ast (ast stream)
  (format stream "   ~a [label = \"~a\"];~%"
          (id ast) (label ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a CONSTANT-AST.

(defmethod stream-draw-ast ((ast constant-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = green];~%" (id ast))
  (format stream "   ~a [label = \"~a\"];~%"
          (id ast)
          (label ast)))

(defmethod label ((ast constant-ast))
  (format nil "~a" (value ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a LEXICAL-AST.

(defmethod stream-draw-ast ((ast lexical-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = yellow];~%" (id ast))
  (format stream "   ~a [label = \"~a\"];~%"
          (id ast)
          (label ast)))

(defmethod label ((ast lexical-ast))
  (format nil "~a" (name (lexical-variable ast))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a TAG-AST.

(defmethod stream-draw-ast ((ast tag-ast) stream)
  (format stream "   ~a [label = \"~a\"];~%"
          (id ast)
          (label ast)))

(defmethod label ((ast tag-ast))
  (format nil "~a" (name ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a TOP-LEVEL-FUNCTION-AST.

(defmethod stream-draw-ast ((ast top-level-function-ast) stream)
  (format stream "   ~a [label = \"~a\"];~%"
          (id ast)
          (label ast)))

(defmethod label ((ast top-level-function-ast))
  (format nil "~a ~a" (call-next-method) (forms ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a LOAD-TIME-VALUE-AST.

(defmethod stream-draw-ast ((ast load-time-value-ast) stream)
  (format stream "   ~a [label = \"~a\"];~%"
          (id ast) (label ast))
  (format stream "   ~a [style = filled, fillcolor = pink];~%"
          (id ast)))

(defmethod label ((ast load-time-value-ast))
  (format nil "~a" (form ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a THE-AST.

(defmethod stream-draw-ast ((ast the-ast) stream)
  (format stream "   ~a [label = \"~a\"];~%"
          (id ast)
          (label ast)))

#+(or)
(defmethod label ((ast the-ast))
  (format nil "the (values ~{~s ~}~@[&optional ~{~s ~}~]&rest ~s)"
          (cleavir-ast:required-types ast)
          (cleavir-ast:optional-types ast)
          (cleavir-ast:rest-type ast)))
