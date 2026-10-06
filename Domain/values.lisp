(in-package #:cleavir-domain)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Domains that pay attention to Lisp's multiple value semantics. Conceptually,
;;; an INFO for such a domain is a description of multiple values, each of which
;;; has its own SV-INFO (single value info), a different kind of thing from the
;;; INFO. This describes, for example, types. The methods in this file produce
;;; the general behavior from the single value methods automatically.

(defclass values-mixin (domain) ())

;;; A values domain that is Noetherian in its single values.
;;; With the default representation of values below, it is not possible to fail
;;; the ascending chain condition if single values meet ACC, but I'm going to be
;;; a bit conservative and define this special mixin rather than noetherian-mixin.
;;; NOTE: So, values-mixin and noetherian-mixin are not compatible.
(defclass noetherian-values-mixin (values-mixin) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Single-value equivalents to the domain lattice functions.
;;;

(defgeneric sv-infimum (client domain))
(defgeneric sv-supremum (client domain))
(defgeneric sv-subinfop (client domain info1 info2))
(defgeneric sv-join/2 (client domain info1 info2))
(defgeneric sv-meet/2 (client domain info1 info2))
(defgeneric sv-widen (client domain old-info new-info))

;;;

(defun sv-join (client domain &rest infos)
  (cond ((null infos) (sv-infimum client domain))
        ((null (rest infos)) (first infos))
        (t (reduce (lambda (i1 i2) (sv-join/2 client domain i1 i2)) infos))))
(define-compiler-macro sv-join (&whole form client domain &rest infos)
  (cond ((null infos) `(sv-infimum ,client ,domain))
        ((and (consp infos)
              (consp (cdr infos))
              (null (cddr infos)))
         `(sv-join/2 ,client ,domain ,(first infos) ,(second infos)))
        (t form)))

(defun sv-meet (client domain &rest infos)
  (cond ((null infos) (sv-supremum client domain))
        ((null (rest infos)) (first infos))
        (t (reduce (lambda (i1 i2) (sv-meet/2 client domain i1 i2)) infos))))
(define-compiler-macro sv-meet (&whole form client domain &rest infos)
  (cond ((null infos) `(sv-supremum ,client ,domain))
        ((and (consp infos)
              (consp (cdr infos))
              (null (cddr infos)))
         `(sv-meet/2 ,client ,domain ,(first infos) ,(second infos)))
        (t form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to create infos from single value infos, and to extract single
;;; value infos from infos.
;;;

;; REQUIRED, OPTIONAL are lists of sv-info. REST is an sv-info.
(defgeneric values-info (client domain required optional rest))

(defgeneric values-required (client domain info))
(defgeneric values-optional (client domain info))
(defgeneric values-rest (client domain info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default implementation, for domains that want to define the single-value
;;; version and not bother thinking about multiple values.
;;;

(defclass values-info ()
  ((%required :initarg :required :reader %required)
   (%optional :initarg :optional :reader %optional)
   (%rest :initarg :rest :reader %rest)))

(defmethod print-object ((obj values-info) stream)
  (print-unreadable-object (obj stream :type t)
    (write `(values ,@(%required obj) &optional ,@(%optional obj)
                    &rest ,(%rest obj))
           :stream stream))
  obj)

(defmethod values-info (client domain required optional rest)
  (declare (ignore client))
  (make-instance 'values-info
    :required required :optional optional :rest rest))
(defmethod values-required (client domain (info values-info))
  (declare (ignore client domain))
  (%required info))
(defmethod values-optional (client domain (info values-info))
  (declare (ignore client domain))
  (%optional info))
(defmethod values-rest (client domain (info values-info))
  (declare (ignore client domain))
  (%rest info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Derived operators.

(defun info-values-nth (client domain i info)
  (let* ((req (values-required client domain info))
         (lreq (length req)))
    (if (< i lreq)
        (nth i req)
        (let ((opt (values-optional client domain info)))
          (if (< i (+ lreq (length opt)))
              (nth (- i lreq) opt)
              (values-rest client domain info))))))

(defun primary (client domain info)
  (let ((req (values-required client domain info)))
    (if (null req)
        (let ((opt (values-optional client domain info)))
          (if (null opt)
              (values-rest client domain info)
              (first opt)))
        (first req))))

(defun single-value (client domain sv-info)
  (values-info client domain (list sv-info) nil (sv-infimum client domain)))

(defun ftm-info (client domain required)
  (values-info client domain required nil (sv-infimum client domain)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Noetherian definition.

(defmethod sv-widen (client (domain noetherian-values-mixin) old-info new-info)
  (declare (ignore client old-info))
  new-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods for the domain lattice functions in terms of the single value
;;; lattice functions.

(defmethod infimum (client (domain values-mixin))
  ;; FIXME: not sure about this! Maybe there should be a dedicated
  ;; values-bottom object? As is, we're essentially saying that the infimum is
  ;; anything with at least one required value that's the infimum, which is
  ;; really kind of awkward.
  (single-value client domain (sv-infimum client domain)))

(defmethod supremum (client (domain values-mixin))
  (values-info client domain nil nil (sv-supremum client domain)))

(defmethod subinfop (client (domain values-mixin) info1 info2)
  (let* ((required1 (values-required client domain info1))
         (required1-count (length required1))
         (optional1 (values-optional client domain info1))
         (rest1 (values-rest client domain info1))
         (required2 (values-required client domain info2))
         (required2-count (length required2))
         (optional2 (values-optional client domain info2))
         (rest2 (values-rest client domain info2)))
    (cond ((< required1-count required2-count) (values nil t))
          ((< (+ required1-count (length optional1))
              (+ required2-count (length optional2)))
           (cl:values nil nil))
          (t
           (labels ((aux (t1 t2)
                      (if (null t2)
                          (sv-subinfop client domain rest1 rest2)
                          (multiple-value-bind (answer certain)
                              (sv-subinfop client domain (first t1) (first t2))
                            (if answer
                                (aux (rest t1) (rest t2))
                                (cl:values nil certain))))))
             (aux (append required1 optional1)
                  (append required2 optional2)))))))

(defmethod join/2 (client (domain values-mixin) info1 info2)
  ;; (the general case below is not minimal)
  (loop with required1 = (values-required client domain info1)
        with optional1 = (values-optional client domain info1)
        with rest1 = (values-rest client domain info1)
        with required2 = (values-required client domain info2)
        with optional2 = (values-optional client domain info2)
        with rest2 = (values-rest client domain info2)
        with required with optional with rest
        with donep = nil
        do (if (null required1)
               (if (null optional1)
                   (if (null required2)
                       (if (null optional2)
                           ;; rest v rest
                           (setf rest (sv-join/2 client domain rest1 rest2)
                                 donep t)
                           ;; rest v opt
                           (push (sv-join/2 client domain rest1 (pop optional2))
                                 optional))
                       ;; rest v req
                       (push (sv-join/2 client domain rest1 (pop required2))
                             optional))
                   (if (null required2)
                       (if (null optional2)
                           ;; optional v rest
                           (push (sv-join/2 client domain (pop optional1) rest2)
                                 optional)
                           ;; optional v optional
                           (push (sv-join/2 client domain
                                            (pop optional1) (pop optional2))
                                 optional))
                       ;; optional v req
                       (push (sv-join/2 client domain
                                        (pop optional1) (pop required2))
                             optional)))
               (if (null required2)
                   (if (null optional2)
                       ;; required v rest
                       (push (sv-join/2 client domain (pop required1) rest2)
                             optional)
                       ;; required v optional
                       (push (sv-join/2 client domain
                                        (pop required1) (pop optional2))
                             optional))
                   ;; required v required
                   (push (sv-join/2 client domain
                                    (pop required1) (pop required2))
                         required)))
        when donep
        return (values-info client domain
                            (nreverse required) (nreverse optional) rest)))

(defmethod widen (client (domain values-mixin) info1 info2)
  ;; (the general case below is not minimal)
  ;; FIXME: We're also not actually Noetherian here, as we can keep adding
  ;; values on to the right.
  (loop with required1 = (values-required client domain info1)
        with optional1 = (values-optional client domain info1)
        with rest1 = (values-rest client domain info1)
        with required2 = (values-required client domain info2)
        with optional2 = (values-optional client domain info2)
        with rest2 = (values-rest client domain info2)
        with required with optional with rest
        with donep = nil
        do (if (null required1)
               (if (null optional1)
                   (if (null required2)
                       (if (null optional2)
                           ;; rest v rest
                           (setf rest (sv-widen client domain rest1 rest2)
                                 donep t)
                           ;; rest v opt
                           (push (sv-widen client domain rest1 (pop optional2))
                                 optional))
                       ;; rest v req
                       (push (sv-widen client domain rest1 (pop required2))
                             optional))
                   (if (null required2)
                       (if (null optional2)
                           ;; optional v rest
                           (push (sv-widen client domain (pop optional1) rest2)
                                 optional)
                           ;; optional v optional
                           (push (sv-widen client domain
                                           (pop optional1) (pop optional2))
                                 optional))
                       ;; optional v req
                       (push (sv-widen client domain
                                       (pop optional1) (pop required2))
                             optional)))
               (if (null required2)
                   (if (null optional2)
                       ;; required v rest
                       (push (sv-widen client domain (pop required1) rest2)
                             optional)
                       ;; required v optional
                       (push (sv-widen client domain
                                       (pop required1) (pop optional2))
                             optional))
                   ;; required v required
                   (push (sv-widen client domain (pop required1) (pop required2))
                         required)))
        when donep
        return (values-info client domain
                            (nreverse required) (nreverse optional) rest)))

(defmethod meet/2 (client (domain values-mixin) info1 info2)
  (loop with required1 = (values-required client domain info1)
        with optional1 = (values-optional client domain info1)
        with rest1 = (values-rest client domain info1)
        with required2 = (values-required client domain info2)
        with optional2 = (values-optional client domain info2)
        with rest2 = (values-rest client domain info2)
        with required with optional with rest
        with donep = nil
        do (if (null required1)
               (if (null optional1)
                   (if (null required2)
                       (if (null optional2)
                           ;; rest v rest
                           (setf rest (sv-meet/2 client domain rest1 rest2)
                                 donep t)
                           ;; rest v opt
                           (push (sv-meet/2 client domain rest1 (pop optional2))
                                 optional))
                       ;; rest v req
                       (push (sv-meet/2 client domain rest1 (pop required2))
                             required))
                   (if (null required2)
                       (if (null optional2)
                           ;; optional v rest
                           (push (sv-meet/2 client domain (pop optional1) rest2)
                                 optional)
                           ;; optional v optional
                           (push (sv-meet/2 client domain
                                            (pop optional1) (pop optional2))
                                 optional))
                       ;; optional v req
                       (push (sv-meet/2 client domain
                                        (pop optional1) (pop required2))
                             required)))
               (if (null required2)
                   (if (null optional2)
                       ;; required v rest
                       (push (sv-meet/2 client domain (pop required1) rest2)
                             required)
                       ;; required v optional
                       (push (sv-meet/2 client domain
                                        (pop required1) (pop optional2))
                             required))
                   ;; required v required
                   (push (sv-meet/2 client domain
                                    (pop required1) (pop required2))
                         required)))
        when donep
        return (values-info client domain
                            (nreverse required) (nreverse optional) rest)))
