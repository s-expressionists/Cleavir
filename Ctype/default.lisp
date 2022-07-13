(in-package #:cleavir-ctype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This is a default implementation of the ctype protocol, for clients that
;;; don't want to bother implementing it themselves.
;;; In this implementation, ctypes are CL type specifiers, though stripped of
;;; environment dependency. CL:SUBTYPEP etc. are used.

;;; Internal: Check whether the given default ctype is a values ctype.
(defun values-ctype-p (ctype)
  (and (cl:consp ctype) (eql (car ctype) 'cl:values)))

(defmethod subtypep (ct1 ct2 sys)
  (declare (ignore sys))
  (cl:subtypep ct1 ct2))

(defmethod upgraded-array-element-type (ct sys)
  (declare (ignore sys))
  (cl:upgraded-array-element-type ct))

(defmethod upgraded-complex-part-type (ct sys)
  (declare (ignore sys))
  (cl:upgraded-complex-part-type ct))

(defmethod values-subtypep (ctype1 ctype2 system)
  (assert (and (values-ctype-p ctype1) (values-ctype-p ctype2))
          () "An argument to ~s is not a values ctype: args are ~s ~s"
          'values-subtypep ctype1 ctype2)
  (let* ((required1 (values-required ctype1 system))
         (required1-count (length required1))
         (optional1 (values-optional ctype1 system))
         (rest1 (values-rest ctype1 system))
         (required2 (values-required ctype2 system))
         (required2-count (length required2))
         (optional2 (values-optional ctype2 system))
         (rest2 (values-rest ctype2 system)))
    (cond ((< required1-count required2-count)
           (cl:values nil t))
          ((< (+ required1-count (length optional1))
              (+ required2-count (length optional2)))
           (cl:values nil nil))
          (t
           (labels ((aux (t1 t2)
                      (if (null t1)
                          (subtypep rest1 rest2 system)
                          (multiple-value-bind (answer certain)
                              (subtypep (first t1) (if t2 (first t2) rest2)
                                        system)
                            (if answer
                                (aux (rest t1) (rest t2))
                                (cl:values nil certain))))))
             (aux (append required1 optional1)
                  (append required2 optional2)))))))

(defmethod top (sys) (declare (ignore sys)) 't)
(defmethod bottom (sys) (declare (ignore sys)) 'nil)

(defmethod top-p (ctype sys)
  (declare (ignore sys))
  (eql ctype 't))

(defmethod bottom-p (ctype sys)
  (declare (ignore sys))
  (eql ctype 'nil))

(defmethod values-top (sys) (values nil nil (top sys) sys))
(defmethod values-bottom (sys)
  ;; Recapitulating the generic function's comment:
  ;; This is really actually definitely not (values &rest nil)!
  (let ((bot (bottom sys)))
    (values (list bot) nil bot sys)))

(defun values-bottom-p (vct sys)
  (some (lambda (ct) (bottom-p ct sys)) (values-required vct sys)))

(defmethod conjunctionp (ctype sys)
  (declare (ignore sys))
  (and (cl:consp ctype) (eq (car ctype) 'and)))
(defmethod conjunction-ctypes (ctype sys)
  (declare (ignore sys))
  (rest ctype))
(defmethod disjunctionp (ctype sys)
  (declare (ignore sys))
  (and (cl:consp ctype) (eq (car ctype) 'or)))
(defmethod disjunction-ctypes (ctype sys)
  (declare (ignore sys))
  (rest ctype))

;;; internal
(defun function-ctype-p (ctype)
  (and (cl:consp ctype) (eq (car ctype) 'cl:function)))
(defun function-return (ctype) (third ctype))

(defmethod values-conjoin/2 (vct1 vct2 sys)
  (assert (and (values-ctype-p vct1) (values-ctype-p vct2))
          () "An argument to ~s is not a values ctype: args are ~s ~s"
          'values-conjoin vct1 vct2)
  (loop with required1 = (values-required vct1 sys)
        with optional1 = (values-optional vct1 sys)
        with rest1 = (values-rest vct1 sys)
        with required2 = (values-required vct2 sys)
        with optional2 = (values-optional vct2 sys)
        with rest2 = (values-rest vct2 sys)
        with required with optional with rest
        with donep = nil
        do (if (null required1)
               (if (null optional1)
                   (if (null required2)
                       (if (null optional2)
                           ;; rest v rest
                           (setf rest (conjoin/2 rest1 rest2 sys)
                                 donep t)
                           ;; rest v opt
                           (push (conjoin/2 rest1 (pop optional2) sys)
                                 optional))
                       ;; rest v req
                       (push (conjoin/2 rest1 (pop required2) sys)
                             required))
                   (if (null required2)
                       (if (null optional2)
                           ;; optional v rest
                           (push (conjoin/2 (pop optional1) rest2 sys)
                                 optional)
                           ;; optional v optional
                           (push (conjoin/2 (pop optional1) (pop optional2) sys)
                                 optional))
                       ;; optional v req
                       (push (conjoin/2 (pop optional1) (pop required2) sys)
                             required)))
               (if (null required2)
                   (if (null optional2)
                       ;; required v rest
                       (push (conjoin/2 (pop required1) rest2 sys)
                             required)
                       ;; required v optional
                       (push (conjoin/2 (pop required1) (pop optional2) sys)
                             required))
                   ;; required v required
                   (push (conjoin/2 (pop required1) (pop required2) sys)
                         required)))
        when donep
        return (if (some (lambda (req) (bottom-p req sys)) required)
                   (values (make-list (length required)
                                      :initial-element (bottom sys))
                           nil nil sys)
                   (values (nreverse required) (nreverse optional) rest sys))))

(defmethod conjoin/2 (ct1 ct2 sys)
  (cond
    ((or (values-ctype-p ct1) (values-ctype-p ct2))
     (error "Values ctypes ~a ~a input to conjoin/2" ct1 ct2))
    ;; Pick off some very basic cases.
    ((or (bottom-p ct1 sys) (bottom-p ct2 sys)) 'nil)
    ((top-p ct1 sys) ct2)
    ((top-p ct2 sys) ct1)
    ((cl:subtypep ct1 ct2) ct1)
    ((cl:subtypep ct2 ct1) ct2)
    (t (let ((ty `(and ,ct1 ,ct2)))
         ;; Checking for bottom-ness is a very basic
         ;; canonicalization we can perform with the
         ;; limited tools CL gives us.
         (if (cl:subtypep ty nil)
             nil
             ty)))))

(defmethod values-disjoin/2 (vct1 vct2 sys)
  (assert (and (values-ctype-p vct1) (values-ctype-p vct2)))
  ;; If either type is bottom, return the other
  ;; (the general case below does not handle bottom types optimally;
  ;;  e.g. (values nil &rest t) (values t t) will disjoin to
  ;;  (values t &optional t))
  (cond ((values-bottom-p vct1 sys)
         (return-from values-disjoin/2 vct2))
        ((values-bottom-p vct2 sys)
         (return-from values-disjoin/2 vct1)))
  ;; General case
  (loop with required1 = (values-required vct1 sys)
        with optional1 = (values-optional vct1 sys)
        with rest1 = (values-rest vct1 sys)
        with required2 = (values-required vct2 sys)
        with optional2 = (values-optional vct2 sys)
        with rest2 = (values-rest vct2 sys)
        with required with optional with rest
        with donep = nil
        do (if (null required1)
               (if (null optional1)
                   (if (null required2)
                       (if (null optional2)
                           ;; rest v rest
                           (setf rest (disjoin/2 rest1 rest2 sys)
                                 donep t)
                           ;; rest v opt
                           (push (disjoin/2 rest1 (pop optional2) sys)
                                 optional))
                       ;; rest v req
                       (push (disjoin/2 rest1 (pop required2) sys)
                             optional))
                   (if (null required2)
                       (if (null optional2)
                           ;; optional v rest
                           (push (disjoin/2 (pop optional1) rest2 sys)
                                 optional)
                           ;; optional v optional
                           (push (disjoin/2 (pop optional1) (pop optional2) sys)
                                 optional))
                       ;; optional v req
                       (push (disjoin/2 (pop optional1) (pop required2) sys)
                             optional)))
               (if (null required2)
                   (if (null optional2)
                       ;; required v rest
                       (push (disjoin/2 (pop required1) rest2 sys)
                             optional)
                       ;; required v optional
                       (push (disjoin/2 (pop required1) (pop optional2) sys)
                             optional))
                   ;; required v required
                   (push (disjoin/2 (pop required1) (pop required2) sys)
                         required)))
        when donep
          return (values (nreverse required) (nreverse optional) rest sys)))

(defmethod values-wdisjoin/2 (vct1 vct2 sys)
  ;; FIXME: This is not actually Noetherian right now, since more values can
  ;; get tacked on indefinitely!
  (assert (and (values-ctype-p vct1) (values-ctype-p vct2)))
  (cond ((values-bottom-p vct1 sys)
         (return-from values-wdisjoin/2 vct2))
        ((values-bottom-p vct2 sys)
         (return-from values-wdisjoin/2 vct1)))
  ;; General case
  (loop with required1 = (values-required vct1 sys)
        with optional1 = (values-optional vct1 sys)
        with rest1 = (values-rest vct1 sys)
        with required2 = (values-required vct2 sys)
        with optional2 = (values-optional vct2 sys)
        with rest2 = (values-rest vct2 sys)
        with required with optional with rest
        with donep = nil
        do (if (null required1)
               (if (null optional1)
                   (if (null required2)
                       (if (null optional2)
                           ;; rest v rest
                           (setf rest (wdisjoin/2 rest1 rest2 sys)
                                 donep t)
                           ;; rest v opt
                           (push (wdisjoin/2 rest1 (pop optional2) sys)
                                 optional))
                       ;; rest v req
                       (push (wdisjoin/2 rest1 (pop required2) sys)
                             optional))
                   (if (null required2)
                       (if (null optional2)
                           ;; optional v rest
                           (push (wdisjoin/2 (pop optional1) rest2 sys)
                                 optional)
                           ;; optional v optional
                           (push (wdisjoin/2 (pop optional1) (pop optional2) sys)
                                 optional))
                       ;; optional v req
                       (push (wdisjoin/2 (pop optional1) (pop required2) sys)
                             optional)))
               (if (null required2)
                   (if (null optional2)
                       ;; required v rest
                       (push (wdisjoin/2 (pop required1) rest2 sys)
                             optional)
                       ;; required v optional
                       (push (wdisjoin/2 (pop required1) (pop optional2) sys)
                             optional))
                   ;; required v required
                   (push (wdisjoin/2 (pop required1) (pop required2) sys)
                         required)))
        when donep
          return (values (nreverse required) (nreverse optional) rest sys)))

(defmethod disjoin/2 (ct1 ct2 sys)
  (cond
    ((or (values-ctype-p ct1) (values-ctype-p ct2))
     (error "values ctypes ~a ~a input to disjoin" ct1 ct2))
    ((top-p ct1 sys) ct1)
    ((top-p ct2 sys) ct2)
    ((bottom-p ct1 sys) ct2)
    ((bottom-p ct2 sys) ct1)
    ((cl:subtypep ct1 ct2) ct2)
    ((cl:subtypep ct2 ct1) ct1)
    (t `(or ,ct1 ,ct2))))

(defmethod wdisjoin/2 (ct1 ct2 sys)
  (cond
    ((top-p ct1 sys) ct1)
    ((top-p ct2 sys) ct2)
    ((bottom-p ct1 sys) ct2)
    ((bottom-p ct2 sys) ct1)
    (t (let ((sum `(or ,ct1 ,ct2)))
         (macrolet ((tcases (&rest type-specifiers)
                      `(cond
                         ,@(loop for ts in type-specifiers
                                 collect `((cl:subtypep sum ',ts) ',ts))
                         (t (top sys)))))
           (tcases cl:nil cl:cons cl:null cl:symbol cl:base-char cl:character
                   cl:hash-table cl:function cl:readtable cl:package
                   cl:pathname cl:stream cl:random-state cl:condition
                   cl:restart cl:structure-object cl:condition
                   cl:standard-object
                   short-float long-float double-float single-float ratio
                   cl:complex
                   bit (unsigned-byte 4) (signed-byte 4)
                   (unsigned-byte 8) (signed-byte 8)
                   (unsigned-byte 16) (signed-byte 16)
                   (unsigned-byte 29) (signed-byte 29)
                   (unsigned-byte 32) (signed-byte 32)
                   (unsigned-byte 61) (signed-byte 61)
                   (unsigned-byte 64) (signed-byte 64)
                   integer rational real
                   simple-vector cl:string (simple-array * (*)) vector
                   simple-array cl:array))))))

(defmethod negate (ct sys)
  (cond ((top-p ct sys) 'nil)
        ((bottom-p ct sys) 't)
        (t `(not ,ct))))

(defmethod subtract (ct1 ct2 sys)
  (cond ((bottom-p ct1 sys) 'nil)
        ((bottom-p ct2 sys) ct1)
        ((top-p ct2 sys) 'nil)
        (t (conjoin/2 ct1 (negate ct2 sys) sys))))

(defmethod values-append/2 (ct1 ct2 system)
  ;; This is considerably complicated by nontrivial &optional and &rest.
  ;; For a start (to be improved? FIXME) we take the required values of the
  ;; first form, and record the minimum number of required values, which is
  ;; just the sum of those of the values types.
  ;; Also, if the number of values of the first type is fixed (no &optional
  ;; and the &rest is bottom) we give the simple exact result.
  (let ((req1 (values-required ct1 system))
        (opt1 (values-optional ct1 system))
        (rest1 (values-rest ct1 system))
        (req2 (values-required ct2 system))
        (opt2 (values-optional ct2 system))
        (rest2 (values-rest ct2 system)))
    (if (and (null opt1) (bottom-p rest1 system))
        ;; simple case
        (values (append req1 req2) opt2 rest2 system)
        ;; Approximate as described
        (values
         (append req1 (make-list (length req2)
                                 :initial-element (top system)))
         nil (top system) system))))

(defun function-returns (fctype) (third fctype))

(defun general-function-returns (fctype system)
  (cond ((function-ctype-p fctype)
         (function-returns fctype))
        ((conjunctionp fctype system)
         (cl:apply #'conjoin system
                   (loop for fc in (conjunction-ctypes fctype system)
                         collect (general-function-returns fc system))))
        ((disjunctionp fctype)
         (cl:apply #'disjoin system
                   (loop for fc in (disjunction-ctypes fctype system)
                         collect (general-function-returns fc system))))
        ;; give up
        (t `(cl:values &rest t))))

(defmethod apply (fctype actype system)
  (declare (ignore actype))
  (general-function-returns fctype system))

(defmethod funcall (system fctype &rest atypes)
  (declare (ignore atypes))
  (general-function-returns fctype system))

(defmethod class (class sys) (declare (ignore sys)) class)

(defmethod cons (car cdr sys)
  (declare (ignore sys))
  (cond ((eql car 'nil) 'nil)
        ((eql cdr 'nil) 'nil)
        (t `(cl:cons ,car ,cdr))))

(defmethod consp (type sys)
  (declare (ignore sys))
  (and (cl:consp type) (eq (car type) 'cl:cons)
       (cl:consp (cdr type))
       (cl:consp (cddr type))
       (cl:null (cdddr type))))
(defmethod cons-car (type sys)
  (declare (ignore sys))
  (second type))
(defmethod cons-cdr (type sys)
  (declare (ignore sys))
  (third type))

(defun normalize-dimensions (dimensions)
  (if (integerp dimensions)
      (make-list dimensions :initial-element '*)
      dimensions))
(defmethod array (element dimensions simplicity sys)
  (declare (ignore sys))
  `(,simplicity ,element ,dimensions))
(defmethod arrayp (type sys)
  ;; FIXME: Doesn't work on string types.
  (declare (ignore sys))
  (and (cl:consp type) (cl:member (car type) '(cl:array cl:simple-array))
       (cl:consp (cdr type))
       (cl:consp (cddr type))
       (cl:null (cdddr type))))
(defmethod array-element-type (type sys)
  (declare (ignore sys))
  (second type))
(defmethod array-dimensions (type sys)
  (declare (ignore sys))
  (third type))

(defmethod string (dimension simplicity sys)
  (declare (ignore sys))
  `(,(ecase simplicity
       ((cl:array) 'cl:string)
       ((cl:simple-array) 'cl:simple-string))
    ,dimension))

(defmethod character (sys) (declare (ignore sys)) 'cl:character)
(defmethod base-char (sys) (declare (ignore sys)) 'cl:base-char)
(defmethod standard-char (sys) (declare (ignore sys)) 'cl:standard-char)

(defmethod complex (part sys)
  (declare (ignore sys))
  `(cl:complex ,part))

(defmethod complexp (type sys)
  (declare (ignore sys))
  (and (cl:consp type) (eq (car type) 'cl:complex)
       (cl:consp (cdr type)) (cl:null (cddr type))))
(defmethod complex-part-type (type sys)
  (declare (ignore sys))
  (second type))

(defmethod range (type low high sys)
  (declare (ignore sys))
  `(,type ,low ,high))
(defmethod rangep (type sys)
  (declare (ignore sys))
  (and (cl:consp type) (cl:member (car type) '(cl:integer cl:ratio cl:rational
                                               cl:float cl:single-float cl:double-float
                                               cl:short-float cl:long-float cl:real))
       (cl:consp (cdr type))
       (cl:consp (cddr type)) (cl:null (cdddr type))))
(defun process-interval-designator (desig)
  (cond ((eq desig '*) (cl:values nil nil))
        ((listp desig) (cl:values (first desig) t))
        (t (cl:values desig nil))))
(defmethod range-kind (type sys)
  (declare (ignore sys))
  (first type))
(defmethod range-low (type sys)
  (declare (ignore sys))
  (process-interval-designator (second type)))
(defmethod range-high (type sys)
  (declare (ignore sys))
  (process-interval-designator (third type)))

(defmethod fixnum (sys) (declare (ignore sys)) 'cl:fixnum)

(defmethod member (sys &rest elems)
  (declare (ignore sys))
  `(cl:member ,@elems))

(defmethod member-p (sys ctype)
  (declare (ignore sys))
  (and (cl:consp ctype) (eq (first ctype) 'cl:member)))
(defmethod member-members (sys ctype)
  (declare (ignore sys))
  (rest ctype))

(defmethod satisfies (fname sys)
  (declare (ignore sys))
  `(cl:satisfies ,fname))

(defmethod keyword (sys) (declare (ignore sys)) 'cl:keyword)

(defmethod function (req opt rest keyp keys aokp returns sys)
  (declare (ignore sys))
  `(cl:function (,@req &optional ,@opt &rest ,rest
                       ,@(when keyp `(&key ,@keys))
                       ,@(when aokp '(&allow-other-keys)))
                ,returns))

(defmethod compiled-function (sys) (declare (ignore sys))
  'cl:compiled-function)

(defmethod values (req opt rest sys)
  (declare (ignore sys))
  (when (or (some #'values-ctype-p req) (some #'values-ctype-p opt)
            (values-ctype-p rest))
    (error "Nested values ctype on ~a ~a ~a" req opt rest))
  `(cl:values ,@req &optional ,@opt &rest ,rest))

;;; These readers work on the premise that these type specifiers
;;; are normalized by the other methods, so they always have
;;; certain lambda list keywords.

(defun ll-required (lambda-list)
  (ldiff lambda-list (cl:member '&optional lambda-list)))

(defun ll-optional (lambda-list)
  (ldiff (cl:rest (cl:member '&optional lambda-list))
         (cl:member '&rest lambda-list)))

(defun ll-rest (lambda-list)
  (second (cl:member '&rest lambda-list)))

(defun ll-keysp (lambda-list)
  (cl:member '&key lambda-list))

(defun ll-keys (lambda-list)
  (let ((res (cl:member '&key lambda-list)))
    (if res
        (ldiff res (cl:member '&allow-other-keys res))
        nil)))

(defun ll-aokp (lambda-list)
  (cl:member '&allow-other-keys lambda-list))

(defmethod values-required (ctype system)
  (declare (ignore system))
  (ll-required (cl:rest ctype)))

(defmethod values-optional (ctype system)
  (declare (ignore system))
  (ll-optional (cl:rest ctype)))

(defmethod values-rest (ctype system)
  (declare (ignore system))
  (ll-rest (cl:rest ctype)))

(defmethod nth-value (n ctype system)
  (let* ((req (values-required ctype system))
         (nreq (length req)))
    (cond ((< n nreq) (nth n req))
          ((some (lambda (ct) (bottom-p ct system)) req) (bottom system))
          (t (disjoin
              system
              (member system nil)
              (let* ((opt (values-optional ctype system))
                     (nopt (length opt)))
                (if (< n (+ nreq nopt))
                    (nth (- n nreq) opt)
                    (values-rest ctype system))))))))

(defmethod function-required (ctype system)
  (declare (ignore system))
  (ll-required (second ctype)))

(defmethod function-optional (ctype system)
  (declare (ignore system))
  (ll-optional (second ctype)))

(defmethod function-rest (ctype system)
  (declare (ignore system))
  (ll-rest (second ctype)))

(defmethod function-keysp (ctype system)
  (declare (ignore system))
  (ll-keysp (second ctype)))

(defmethod function-keys (ctype system)
  (declare (ignore system))
  (ll-keys (second ctype)))

(defmethod function-allow-other-keys-p (ctype system)
  (declare (ignore system))
  (ll-aokp (second ctype)))

(defmethod function-values (ctype system)
  (declare (ignore system))
  (third ctype))
