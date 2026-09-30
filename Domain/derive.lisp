(in-package #:cleavir-domain)

;;;; A DERIVER is the abstraction of a function (in the general sense, so
;;;; including Lisp functions but potentially instructions as well). Given
;;;; infos from various domains, representing the inputs and outputs, the
;;;; deriver computes the info from one domain representing the output or
;;;; input (depending on the nature of the domain).
;;;; For example, type derivers compute the result type given the type of
;;;; the arguments. But derivers for other domains are possible as well,
;;;; e.g. computing reachability - whether a function returns - given the
;;;; types of the arguments, or other information.

;;;; Derivers necessarily can perform arbitrary computations (although they
;;;; should always halt) so they are Lisp functions. They return an info
;;;; for whatever domain they target. As an argument they get a product
;;;; info. If the product doesn't have some info they want they can still
;;;; request it and just get the supremum (see product.lisp).

(defun destructure-basic-lambda-list (lambda-list)
  (let* ((subkey (member '&key lambda-list))
         (key (rest subkey))
         (subrest (member '&rest lambda-list))
         (rest (second subrest))
         (subopt (member '&optional lambda-list))
         (opt (ldiff (rest subopt) (or subrest subkey)))
         (req (ldiff lambda-list (or subopt subrest subkey))))
    (values req opt rest subkey key)))

(defun normalize-optional (client-var domain-var optional)
  (when (symbolp optional)
    (return-from normalize-optional
      (values optional `(sv-supremum ,client-var ,domain-var) nil)))
  (unless (and (consp optional) (consp (cdr optional))
               (symbolp (first optional))
               (or (null (cddr optional))
                   (and (consp (cddr optional)) (symbolp (caddr optional))
                        (null (cdddr optional)))))
    (error "Invalid OPTIONAL parameter: must be (symbol default [requiredp]), not ~s"
           optional))
  (values (first optional) (second optional) (third optional)))

(defun normalize-key (client-var domain-var key)
  (when (symbolp key)
    (return-from normalize-key
      (values (intern (symbol-name key) "KEYWORD") key
              `(sv-supremum ,client-var ,domain-var))))
  (unless (and (consp key) (consp (cdr key)) (null (cddr key)))
    (error "Invalid KEY parameter: must be (symbol default [requiredp]) or ((key symbol) default [requiredp]), not ~s" key))
  (cond ((symbolp (first key))
         (values (intern (symbol-name (first key)) "KEYWORD")
                 (first key) (second key)))
        ((and (consp (first key)) (consp (cdr (first key)))
              (null (cddr (first key)))
              (symbolp (first (first key)))
              (symbolp (second (first key))))
         (values (first (first key)) (second (first key)) (second key)))
        (t (error "Invalid KEY parameter: must be (symbol default) or ((key symbol) default), not ~s" key))))

(defun domain-bindings (client domain info lambda-list)
  (if (symbolp lambda-list)
      (values `((,lambda-list ,info)) ())
      (let ((ginfo (gensym "INFO"))
            (vreq (gensym "REQUIRED")) (vopt (gensym "OPTIONAL"))
            (vrest (gensym "REST")))
        (values
         (multiple-value-bind (req opt rest keyp)
             (destructure-basic-lambda-list lambda-list)
           (when keyp (error "~s not allowed in lambda list for ~a"
                             '&key domain))
           (nconc
            (list `(,ginfo ,info)
                  `(,vreq (values-required ,client ,domain ,ginfo))
                  `(,vopt (values-optional ,client ,domain ,ginfo))
                  `(,vrest (values-rest ,client ,domain ,ginfo)))
            (loop for r in req
                  collect `(,r (cond (,vreq (pop ,vreq))
                                     (,vopt (pop ,vopt))
                                     (t ,vrest))))
            (loop for o in opt
                  nconc (multiple-value-bind (var default requiredp)
                            (normalize-optional client domain o)
                          (nconc
                           (if requiredp
                               (list `(,requiredp ,vreq))
                               ())
                           (list
                            `(,var (cond (,vreq (pop ,vreq))
                                         (,vopt
                                          (sv-join ,client (pop ,vopt) ,default))
                                         (t
                                          (sv-join ,client ,vrest ,default))))))))
            (when rest
              (list `(,rest (values-info ,client ,domain
                                         ,vreq ,vopt ,vrest))))))
         (list ginfo vreq vopt vrest)))))

;;; Bind info from multiple domains simultaneously within a body, the info
;;; being projected from a product domain.
;;; Specs is a plist where the keys are domain names. Each value is either a
;;; symbol, in which case the whole info for that domain will be bound to that
;;; symbol, or a lambda-list with only required, &optional, and &rest
;;; arguments and where each &optional is only a symbol. In the lambda list
;;; case, the domain must have values-mixin. The lambda list is destructured
;;; against the info for that domain, such that the required and optional
;;; variables are bound to the corresponding single-value infos, and any rest
;;; argument is bound to a multiple value info for that domain describing all
;;; the remaining values.
;;; Where normal lambda lists have a suppliedp variable for &optional/&key,
;;; these have a "requiredp" variable. This is true if the given argument is
;;; certainly supplied and false if it's not supplied or only possibly supplied.
(defmacro with-info ((client &rest specs &key &allow-other-keys)
                     pdomain info &body body)
  (let ((ginfo (gensym "INFO")))
    (multiple-value-bind (bindings ignorable)
        (loop with bindings = ()
              with ignorable = ()
              for (domain-name ll) on specs by #'cddr
              for info = `(project ,client ,pdomain ,domain-name ,ginfo)
              do (multiple-value-bind (sub-bindings ign)
                     (domain-bindings client domain-name info ll)
                   (setf bindings (nconc bindings sub-bindings)
                         ignorable (nconc ignorable ign)))
              finally (return (values bindings ignorable)))
      `(let* ((,client ,client) (,ginfo ,info)
              ,@bindings)
         (declare (ignorable ,ginfo ,@ignorable))
         ,@body))))

(defun kwarg-type (client keyword default required optional rest)
  (loop with done = nil with reqp = t
        with kwtype = (ctype:member client keyword)
        with result = (ctype:bottom client)
        for keyt = (cond (required (pop required))
                         (optional (setf reqp nil) (pop optional))
                         (rest (setf reqp nil done t) rest))
        for valuet = (cond (required (pop required))
                           (optional (pop optional))
                           (rest))
        if (ctype:subtypep keyt kwtype client)
          ;; this one is DEFINITELY the keyword, so we're done
          return (if reqp ; and it's definitely provided
                     (ctype:disjoin client valuet result)
                     (ctype:disjoin client valuet result default))
        if (not (ctype:disjointp keyt kwtype client)) ; could be the key
          do (setf result (ctype:disjoin client valuet result))
        if done
          return (ctype:disjoin client result default)))

(defun type-domain-bindings (client info lambda-list default)
  (if (symbolp lambda-list)
      (values `((,lambda-list ,info)) ())
      (let ((ginfo (gensym "INFO")) (domain 'type)
            (vreq (gensym "REQUIRED")) (vopt (gensym "OPTIONAL"))
            (vrest (gensym "REST")) (check (gensym "TOO-MANY-ARGS")))
        (multiple-value-bind (req opt rest keysp key)
            (destructure-basic-lambda-list lambda-list)
          (values
           (nconc
            (list `(,ginfo ,info)
                  `(,vreq (values-required ,client ,domain ,ginfo))
                  `(,vopt (values-optional ,client ,domain ,ginfo))
                  `(,vrest (values-rest ,client ,domain ,ginfo)))
            (loop for r in req
                  collect `(,r (let ((,r (cond (,vreq (pop ,vreq))
                                               (,vopt (pop ,vopt))
                                               (t ,vrest))))
                                 (if (ctype:bottom-p ,r ,client)
                                     ,default
                                     ,r))))
            (loop for o in opt
                  nconc (multiple-value-bind (var default requiredp)
                            (normalize-optional client domain o)
                          (nconc
                           (if requiredp
                               (list `(,requiredp ,vreq))
                               ())
                           (list
                            `(,var (if ,vreq
                                       (pop ,vreq)
                                       (sv-join ,client ,domain ,default
                                                (if ,vopt (pop ,vopt) ,vrest))))))))
            (when rest
              (list `(,rest (values-info ,client ,domain
                                         ,vreq ,vopt ,vrest))))
            (when (and (not rest) (not keysp))
              (list `(,check (unless (every (lambda (ty)
                                              (ctype:bottom-p ty ,client))
                                            ,vreq)
                               ,default))))
            (loop for k in key
                  collect (multiple-value-bind (keyword var default)
                              (normalize-key client domain k)
                            `(,var (kwarg-type ,client ',keyword ,default
                                               ,vreq ,vopt ,vrest)))))
           (if (and (not rest) (not keysp))
               (list ginfo vreq vopt vrest check)
               (list ginfo vreq vopt vrest)))))))

;;; Like WITH-INFO but the type domain is treated specially. Instead of being
;;; in SPECS, TYPE must be a symbol or a lambda list. The lambda list can have
;;; &key but all variables must still be symbols. This lambda list is
;;; destructured against the type info for the domain.
;;; If the lambda list cannot match the type info, for example because the
;;; info is the infimum, DEFAULT is evaluated and returned instead of the BODY.
;;; Otherwise like WITH-INFO.
(defmacro with-info-type ((client type default &rest specs
                           &key &allow-other-keys)
                          pdomain info &body body)
  (let ((ginfo (gensym "INFO"))
        (bname (gensym "BLOCK")))
    (multiple-value-bind (bindings ignorable)
        (loop with bindings = ()
              with ignorable = ()
              for (domain-name ll) on specs by #'cddr
              for info = `(project ,client ,pdomain ,domain-name ,ginfo)
              do (multiple-value-bind (sub-bindings ign)
                     (domain-bindings client domain-name info ll)
                   (setf bindings (nconc bindings sub-bindings)
                         ignorable (nconc ignorable ign)))
              finally (return (values bindings ignorable)))
      (multiple-value-bind (type-bindings type-ignorable)
          (type-domain-bindings client
                                `(project ,client ,pdomain type ,ginfo)
                                type `(return-from ,bname ,default))
        `(block ,bname
           (let* ((,client ,client) (,ginfo ,info)
                  ,@type-bindings
                  ,@bindings)
             (declare (ignorable ,ginfo ,@type-ignorable ,@ignorable))
             ,@body))))))

(defmacro deriver-lambda ((client block-name domain type &rest specs)
                          &body body)
  (let ((pdomain (gensym "PRODUCT-DOMAIN")) (info (gensym "PRODUCT-INFO")))
    `(lambda (,client ,pdomain ,info)
       (block ,block-name
         (with-info-type (,client ,type (infimum ,client ,domain) ,@specs)
             ,pdomain ,info
           ,@body)))))
