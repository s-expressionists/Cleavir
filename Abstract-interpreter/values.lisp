(in-package #:cleavir-abstract-interpreter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Values domain extension functions.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Single-value equivalents to the domain lattice functions.
;;;

(defgeneric sv-subinfop (domain info1 info2))
(defgeneric sv-meet/2 (domain info1 info2))
(defgeneric sv-join/2 (domain info1 info2))
(defgeneric sv-widen (domain old-info new-info))
(defgeneric sv-infimum (domain))
(defgeneric sv-supremum (domain))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to create infos from single value infos, and to extract single
;;; value infos from infos.
;;;

;; REQUIRED, OPTIONAL are lists of sv-info. REST is an sv-info.
(defgeneric values-info (domain required optional rest))

(defgeneric values-required (domain info))
(defgeneric values-optional (domain info))
(defgeneric values-rest (domain info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default implementation, for domains that want to define the single-value
;;; version and not bother thinking about multiple values.
;;;

(defclass values-info ()
  ((%required :initarg :required :reader %required)
   (%optional :initarg :optional :reader %optional)
   (%rest :initarg :rest :reader %rest)))

(defmethod values-info (domain required optional rest)
  (make-instance 'values-info
    :required required :optional optional :rest rest))
(defmethod values-required (domain (info values-info)) (%required info))
(defmethod values-optional (domain (info values-info)) (%optional info))
(defmethod values-rest (domain (info values-info)) (%rest info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Derived operators.

(defun info-values-nth (domain i info)
  (let* ((req (values-required domain info))
         (lreq (length req)))
    (if (< i lreq)
        (nth i req)
        (let ((opt (values-optional domain info)))
          (if (< i (+ lreq (length opt)))
              (nth (- i lreq) opt)
              (values-rest domain info))))))

(defun primary (domain info)
  (let ((req (values-required domain info)))
    (if (null req)
        (let ((opt (values-optional domain info)))
          (if (null opt)
              (values-rest domain info)
              (first opt)))
        (first req))))

(defun single-value (domain sv-info)
  (values-info domain (list sv-info) nil (sv-infimum domain)))

(defun ftm-info (domain required)
  (values-info domain required nil (sv-infimum domain)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Noetherian definition.

(defmethod sv-widen ((domain noetherian-mixin) old-info new-info)
  (declare (ignore old-info))
  new-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods for the domain lattice functions in terms of the single value
;;; lattice functions.

(defmethod infimum ((domain values-domain))
  ;; FIXME: not sure about this! Maybe there should be a dedicated
  ;; values-bottom object? As is, we're essentially saying that the infimum is
  ;; anything with at least one required value that's the infimum, which is
  ;; really kind of awkward.
  (single-value domain (sv-infimum domain)))

(defmethod supremum ((domain values-domain))
  (values-info domain nil nil (sv-supremum domain)))

(defmethod subinfop ((domain values-domain) info1 info2)
  (let* ((required1 (values-required domain info1))
         (required1-count (length required1))
         (optional1 (values-optional domain info1))
         (rest1 (values-rest domain info1))
         (required2 (values-required domain info2))
         (required2-count (length required2))
         (optional2 (values-optional domain info2))
         (rest2 (values-rest domain info2)))
    (cond ((< required1-count required2-count) (values nil t))
          ((< (+ required1-count (length optional1))
              (+ required2-count (length optional2)))
           (cl:values nil nil))
          (t
           (labels ((aux (t1 t2)
                      (if (null t2)
                          (sv-subinfop domain rest1 rest2)
                          (multiple-value-bind (answer certain)
                              (sv-subinfop domain (first t1) (first t2))
                            (if answer
                                (aux (rest t1) (rest t2))
                                (cl:values nil certain))))))
             (aux (append required1 optional1)
                  (append required2 optional2)))))))

(defmethod join/2 ((domain values-domain) info1 info2)
  ;; (the general case below is not minimal)
  (loop with required1 = (values-required domain info1)
        with optional1 = (values-optional domain info1)
        with rest1 = (values-rest domain info1)
        with required2 = (values-required domain info2)
        with optional2 = (values-optional domain info2)
        with rest2 = (values-rest domain info2)
        with required with optional with rest
        with donep = nil
        do (if (null required1)
               (if (null optional1)
                   (if (null required2)
                       (if (null optional2)
                           ;; rest v rest
                           (setf rest (sv-join/2 domain rest1 rest2)
                                 donep t)
                           ;; rest v opt
                           (push (sv-join/2 domain rest1 (pop optional2))
                                 optional))
                       ;; rest v req
                       (push (sv-join/2 domain rest1 (pop required2))
                             optional))
                   (if (null required2)
                       (if (null optional2)
                           ;; optional v rest
                           (push (sv-join/2 domain (pop optional1) rest2)
                                 optional)
                           ;; optional v optional
                           (push (sv-join/2 domain
                                            (pop optional1) (pop optional2))
                                 optional))
                       ;; optional v req
                       (push (sv-join/2 domain (pop optional1) (pop required2))
                             optional)))
               (if (null required2)
                   (if (null optional2)
                       ;; required v rest
                       (push (sv-join/2 domain (pop required1) rest2)
                             optional)
                       ;; required v optional
                       (push (sv-join/2 domain (pop required1) (pop optional2))
                             optional))
                   ;; required v required
                   (push (sv-join/2 domain (pop required1) (pop required2))
                         required)))
        when donep
        return (values-info domain
                            (nreverse required) (nreverse optional) rest)))

(defmethod widen ((domain values-domain) info1 info2)
  ;; (the general case below is not minimal)
  ;; FIXME: We're also not actually Noetherian here, as we can keep adding
  ;; values on to the right.
  (loop with required1 = (values-required domain info1)
        with optional1 = (values-optional domain info1)
        with rest1 = (values-rest domain info1)
        with required2 = (values-required domain info2)
        with optional2 = (values-optional domain info2)
        with rest2 = (values-rest domain info2)
        with required with optional with rest
        with donep = nil
        do (if (null required1)
               (if (null optional1)
                   (if (null required2)
                       (if (null optional2)
                           ;; rest v rest
                           (setf rest (sv-widen domain rest1 rest2)
                                 donep t)
                           ;; rest v opt
                           (push (sv-widen domain rest1 (pop optional2))
                                 optional))
                       ;; rest v req
                       (push (sv-widen domain rest1 (pop required2))
                             optional))
                   (if (null required2)
                       (if (null optional2)
                           ;; optional v rest
                           (push (sv-widen domain (pop optional1) rest2)
                                 optional)
                           ;; optional v optional
                           (push (sv-widen domain
                                            (pop optional1) (pop optional2))
                                 optional))
                       ;; optional v req
                       (push (sv-widen domain (pop optional1) (pop required2))
                             optional)))
               (if (null required2)
                   (if (null optional2)
                       ;; required v rest
                       (push (sv-widen domain (pop required1) rest2)
                             optional)
                       ;; required v optional
                       (push (sv-widen domain (pop required1) (pop optional2))
                             optional))
                   ;; required v required
                   (push (sv-widen domain (pop required1) (pop required2))
                         required)))
        when donep
        return (values-info domain
                            (nreverse required) (nreverse optional) rest)))

(defmethod meet/2 ((domain values-domain) info1 info2)
  (loop with required1 = (values-required domain info1)
        with optional1 = (values-optional domain info1)
        with rest1 = (values-rest domain info1)
        with required2 = (values-required domain info2)
        with optional2 = (values-optional domain info2)
        with rest2 = (values-rest domain info2)
        with required with optional with rest
        with donep = nil
        do (if (null required1)
               (if (null optional1)
                   (if (null required2)
                       (if (null optional2)
                           ;; rest v rest
                           (setf rest (sv-meet/2 domain rest1 rest2)
                                 donep t)
                           ;; rest v opt
                           (push (sv-meet/2 domain rest1 (pop optional2))
                                 optional))
                       ;; rest v req
                       (push (sv-meet/2 domain rest1 (pop required2))
                             required))
                   (if (null required2)
                       (if (null optional2)
                           ;; optional v rest
                           (push (sv-meet/2 domain (pop optional1) rest2)
                                 optional)
                           ;; optional v optional
                           (push (sv-meet/2 domain
                                            (pop optional1) (pop optional2))
                                 optional))
                       ;; optional v req
                       (push (sv-meet/2 domain (pop optional1) (pop required2))
                             required)))
               (if (null required2)
                   (if (null optional2)
                       ;; required v rest
                       (push (sv-meet/2 domain (pop required1) rest2)
                             required)
                       ;; required v optional
                       (push (sv-meet/2 domain (pop required1) (pop optional2))
                             required))
                   ;; required v required
                   (push (sv-meet/2 domain (pop required1) (pop required2))
                         required)))
        when donep
        return (values-info domain
                            (nreverse required) (nreverse optional) rest)))
