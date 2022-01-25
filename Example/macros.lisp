(in-package #:cleavir-example)

(defvar *macros* nil)

(defmacro define-example-macro (name lambda-list &body body)
  `(push (cons ',name
               (lambda (form env)
                 (declare (ignorable env))
                 (block ,name
                   (destructuring-bind ,lambda-list
                       (rest form)
                     ,@body))))
         *macros*))

(define-example-macro and (&rest forms)
  (cond ((null forms) 't)
        ((null (rest forms)) (first forms))
        (t `(if ,(first forms) (and ,@(rest forms)) nil))))
(define-example-macro or (&rest forms)
  (cond ((null forms) 'nil)
        ((null (rest forms)) (first forms))
        (t (let ((s (gensym "VAL")))
             `(let ((,s ,(first forms)))
                (if ,s ,s (or ,@(rest forms))))))))

(define-example-macro cond (&rest clauses)
  (if (null clauses)
      'nil
      (let ((clause (first clauses)))
        (if (null (rest clause))
            (let ((s (gensym "VAL")))
              `(let ((,s ,(first clause)))
                 (if ,s ,s (cond ,@(rest clauses)))))
            `(if ,(first clause)
                 (progn ,@(rest clause))
                 (cond ,@(rest clauses)))))))

(define-example-macro dolist ((var list &optional result) &body body)
  (let ((lsym (gensym "LIST"))
        (loop (gensym "LOOP"))
        (end (gensym "END")))
    `(prog ((,lsym ,list) ,var)
      ,loop
        (when (endp ,lsym) (go ,end))
        (setq ,var (car ,lsym))
        ,@body
        (setq ,lsym (cdr ,lsym))
        (go ,loop)
      ,end
        (return ,result))))
(define-example-macro dotimes ((var count &optional result) &body body)
  (let ((csym (gensym "COUNT"))
        (loop (gensym "LOOP"))
        (end (gensym "END")))
    `(prog ((,csym ,count) (,var 0))
      ,loop
        (when (= ,var ,csym) (go ,end))
        ,@body
        (setq ,var (1+ ,var))
        (go ,loop)
      ,end
        (return ,result))))

(define-example-macro lambda (lambda-list &body body)
  `(function (lambda ,lambda-list ,@body)))

(define-example-macro multiple-value-bind (vars form &body body)
  (let ((ign (gensym "IGNORE")))
    `(primop:multiple-value-call
         (lambda (&optional ,@vars &rest ,ign)
           (declare (ignore ,ign))
           ,@body)
       ,form)))
;;; Defined as a macro to take care of function designators.
(define-example-macro multiple-value-call (function &rest argforms)
  (let ((fsym (gensym "FUNCTION")))
    `(let ((,fsym ,function))
       (primop:multiple-value-call
           (if (functionp ,fsym) ,fsym (fdefinition ,fsym))
         ,@argforms))))
(define-example-macro multiple-value-list (values-form)
  `(primop:multiple-value-call #'list ,values-form))

(define-example-macro prog ((&rest bindings) &body body)
  `(block nil (let (,@bindings) (tagbody ,@body))))
(define-example-macro prog* ((&rest bindings) &body body)
  `(block nil (let* (,@bindings) (tagbody ,@body))))

(define-example-macro prog1 (result &body body)
  (let ((s (gensym "RESULT")))
    `(let ((,s ,result)) ,@body ,s)))
(define-example-macro prog2 (first result &body body)
  (let ((s (gensym "RESULT")))
    `(progn ,first (let ((,s ,result)) ,@body ,s))))

(define-example-macro return (&optional result)
  `(return-from ,nil ,result))

(define-example-macro unless (condition &body body)
  `(if ,condition nil (progn ,@body)))
(define-example-macro when (condition &body body)
  `(if ,condition (progn ,@body) nil))
