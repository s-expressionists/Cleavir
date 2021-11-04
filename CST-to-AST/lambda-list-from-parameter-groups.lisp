(cl:in-package #:cleavir-cst-to-ast)

(defun var-to-lexical (var-cst)
  (let* ((raw (cst:raw var-cst))
         (origin (cst:source var-cst))
         (name (make-symbol (string-downcase raw))))
    (ast:make-lexical-variable name :origin origin)))

(defun init-var-to-lexicals (var-cst supplied-p-cst)
  (list (var-to-lexical var-cst)
        (if (null supplied-p-cst)
            (ast:make-lexical-variable (gensym) :origin (cst:source var-cst))
            (var-to-lexical supplied-p-cst))))

(defgeneric entries-from-parameter-group (parameter-group))

(defmethod entries-from-parameter-group
    ((parameter-group cst:multi-parameter-group-mixin))
  (mapcar #'entry-from-parameter (cst:parameters parameter-group)))

(defmethod entries-from-parameter-group
    ((parameter-group cst:ordinary-rest-parameter-group))
  (list (entry-from-parameter (cst:parameter parameter-group))))

(defmethod entries-from-parameter-group
    ((parameter-group cst:aux-parameter-group))
  ;; Don't need any.
  nil)

(defgeneric entry-from-parameter (parameter))

(defmethod entry-from-parameter ((parameter cst:simple-variable))
  (var-to-lexical (cst:name parameter)))

(defmethod entry-from-parameter ((parameter cst:ordinary-optional-parameter))
  (init-var-to-lexicals (cst:name parameter) (cst:supplied-p parameter)))

(defmethod entry-from-parameter ((parameter cst:ordinary-key-parameter))
  (init-var-to-lexicals (cst:name parameter) (cst:supplied-p parameter)))

(defgeneric lambda-list-from-parameter-group (parameter-group entries system))

(defmethod lambda-list-from-parameter-group
    ((parameter-group cst:ordinary-required-parameter-group) entries system)
  (list (lambda-list:make-required-parameter-group entries system)))

(defmethod lambda-list-from-parameter-group
    ((parameter-group cst:optional-parameter-group) entries system)
  (list (lambda-list:make-optional-parameter-group
         (loop for (var supplied) in entries
               collect (lambda-list:make-optional-parameter var supplied system))
         system)))

(defmethod lambda-list-from-parameter-group
    ((parameter-group cst:ordinary-rest-parameter-group) entries system)
  (list (lambda-list:make-rest-parameter-group (first entries) system)))

(defmethod lambda-list-from-parameter-group
    ((parameter-group cst:key-parameter-group) entries system)
  (list (lambda-list:make-key-parameter-group
         (loop for (var supplied) in entries
               for parameter in (cst:parameters parameter-group)
               collect (lambda-list:make-key-parameter
                        (cst:raw (cst:keyword parameter)) var supplied system))
         (cst:allow-other-keys parameter-group)
         system)))

(defmethod lambda-list-from-parameter-group
    ((parameter-group cst:aux-parameter-group) entries system)
  (declare (ignore entries system))
  ;; &aux doesn't contribute to the function-ast's lambda-list.
  nil)

;;; Given a list of parameter groups, return a lambda list suitable
;;; for the FUNCTION-AST, as well as a list of lists of lexical variables.
(defun lambda-list-from-parameter-groups (parameter-groups system)
  (loop for group in parameter-groups
        for entries = (entries-from-parameter-group group)
        for lambda-list-part
          = (lambda-list-from-parameter-group group entries system)
        appending lambda-list-part into lambda-list
        collecting entries into components
        finally (return (values (lambda-list:make-lambda-list
                                 lambda-list system)
                                components))))
