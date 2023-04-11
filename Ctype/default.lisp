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

(defmethod subtypep (client ct1 ct2)
  (declare (ignore client))
  (cl:subtypep ct1 ct2))

(defmethod upgraded-array-element-type (client ct)
  (declare (ignore client))
  (cl:upgraded-array-element-type ct))

(defmethod upgraded-complex-part-type (client ct)
  (declare (ignore client))
  (cl:upgraded-complex-part-type ct))

(defmethod values-subtypep (client ctype1 ctype2)
  (assert (and (values-ctype-p ctype1) (values-ctype-p ctype2))
          () "An argument to ~s is not a values ctype: args are ~s ~s"
          'values-subtypep ctype1 ctype2)
  (let* ((required1 (values-required client ctype1))
         (required1-count (length required1))
         (optional1 (values-optional client ctype1))
         (rest1 (values-rest client ctype1))
         (required2 (values-required client ctype2))
         (required2-count (length required2))
         (optional2 (values-optional client ctype2))
         (rest2 (values-rest client ctype2)))
    (cond ((< required1-count required2-count)
           (cl:values nil t))
          ((< (+ required1-count (length optional1))
              (+ required2-count (length optional2)))
           (cl:values nil nil))
          (t
           (labels ((aux (t1 t2)
                      (if (null t1)
                          (subtypep client rest1 rest2)
                          (multiple-value-bind (answer certain)
                              (subtypep client (first t1)
                                        (if t2 (first t2) rest2))
                            (if answer
                                (aux (rest t1) (rest t2))
                                (cl:values nil certain))))))
             (aux (append required1 optional1)
                  (append required2 optional2)))))))

(defmethod top (client) (declare (ignore client)) 't)
(defmethod bottom (client) (declare (ignore client)) 'nil)

(defmethod top-p (client ctype)
  (declare (ignore client))
  (eql ctype 't))

(defmethod bottom-p (client ctype)
  (declare (ignore client))
  (eql ctype 'nil))

(defmethod values-top (client) (values client nil nil (top client)))
(defmethod values-bottom (client)
  ;; Recapitulating the generic function's comment:
  ;; This is really actually definitely not (values &rest nil)!
  (let ((bot (bottom client)))
    (values client (list bot) nil bot)))

(defun values-bottom-p (client vct)
  (some (lambda (ct) (bottom-p client ct)) (values-required client vct)))

(defmethod conjunctionp (client ctype)
  (declare (ignore client))
  (and (cl:consp ctype) (eq (car ctype) 'and)))
(defmethod conjunction-ctypes (client ctype)
  (declare (ignore client))
  (rest ctype))
(defmethod disjunctionp (client ctype)
  (declare (ignore client))
  (and (cl:consp ctype) (eq (car ctype) 'or)))
(defmethod disjunction-ctypes (client ctype)
  (declare (ignore client))
  (rest ctype))

;;; internal
(defun function-ctype-p (ctype)
  (and (cl:consp ctype) (eq (car ctype) 'cl:function)))
(defun function-return (ctype) (third ctype))

(defmethod values-conjoin/2 (client vct1 vct2)
  (assert (and (values-ctype-p vct1) (values-ctype-p vct2))
          () "An argument to ~s is not a values ctype: args are ~s ~s"
          'values-conjoin vct1 vct2)
  (loop with required1 = (values-required client vct1)
        with optional1 = (values-optional client vct1)
        with rest1 = (values-rest client vct1)
        with required2 = (values-required client vct2)
        with optional2 = (values-optional client vct2)
        with rest2 = (values-rest client vct2)
        with required with optional with rest
        with donep = nil
        do (if (null required1)
               (if (null optional1)
                   (if (null required2)
                       (if (null optional2)
                           ;; rest v rest
                           (setf rest (conjoin/2 client rest1 rest2)
                                 donep t)
                           ;; rest v opt
                           (push (conjoin/2 client rest1 (pop optional2))
                                 optional))
                       ;; rest v req
                       (push (conjoin/2 client rest1 (pop required2))
                             required))
                   (if (null required2)
                       (if (null optional2)
                           ;; optional v rest
                           (push (conjoin/2 client (pop optional1) rest2)
                                 optional)
                           ;; optional v optional
                           (push (conjoin/2 client (pop optional1) (pop optional2))
                                 optional))
                       ;; optional v req
                       (push (conjoin/2 client (pop optional1) (pop required2))
                             required)))
               (if (null required2)
                   (if (null optional2)
                       ;; required v rest
                       (push (conjoin/2 client (pop required1) rest2)
                             required)
                       ;; required v optional
                       (push (conjoin/2 client (pop required1) (pop optional2))
                             required))
                   ;; required v required
                   (push (conjoin/2 client (pop required1) (pop required2))
                         required)))
        when donep
        return (if (some (lambda (req) (bottom-p client req)) required)
                   (values client
                           (make-list (length required)
                                      :initial-element (bottom client))
                           nil nil)
                   (values client (nreverse required) (nreverse optional) rest))))

(defmethod conjoin/2 (client ct1 ct2)
  (cond
    ((or (values-ctype-p ct1) (values-ctype-p ct2))
     (error "Values ctypes ~a ~a input to conjoin/2" ct1 ct2))
    ;; Pick off some very basic cases.
    ((or (bottom-p client ct1) (bottom-p client ct2)) 'nil)
    ((top-p client ct1) ct2)
    ((top-p client ct2) ct1)
    ((cl:subtypep ct1 ct2) ct1)
    ((cl:subtypep ct2 ct1) ct2)
    (t (let ((ty `(and ,ct1 ,ct2)))
         ;; Checking for bottom-ness is a very basic
         ;; canonicalization we can perform with the
         ;; limited tools CL gives us.
         (if (cl:subtypep ty nil)
             nil
             ty)))))

(defmethod values-disjoin/2 (client vct1 vct2)
  (assert (and (values-ctype-p vct1) (values-ctype-p vct2)))
  ;; If either type is bottom, return the other
  ;; (the general case below does not handle bottom types optimally;
  ;;  e.g. (values nil &rest t) (values t t) will disjoin to
  ;;  (values t &optional t))
  (cond ((values-bottom-p client vct1)
         (return-from values-disjoin/2 vct2))
        ((values-bottom-p client vct2)
         (return-from values-disjoin/2 vct1)))
  ;; General case
  (loop with required1 = (values-required client vct1)
        with optional1 = (values-optional client vct1)
        with rest1 = (values-rest client vct1)
        with required2 = (values-required client vct2)
        with optional2 = (values-optional client vct2)
        with rest2 = (values-rest client vct2)
        with required with optional with rest
        with donep = nil
        do (if (null required1)
               (if (null optional1)
                   (if (null required2)
                       (if (null optional2)
                           ;; rest v rest
                           (setf rest (disjoin/2 client rest1 rest2)
                                 donep t)
                           ;; rest v opt
                           (push (disjoin/2 client rest1 (pop optional2))
                                 optional))
                       ;; rest v req
                       (push (disjoin/2 client rest1 (pop required2))
                             optional))
                   (if (null required2)
                       (if (null optional2)
                           ;; optional v rest
                           (push (disjoin/2 client (pop optional1) rest2)
                                 optional)
                           ;; optional v optional
                           (push (disjoin/2 client (pop optional1) (pop optional2))
                                 optional))
                       ;; optional v req
                       (push (disjoin/2 client (pop optional1) (pop required2))
                             optional)))
               (if (null required2)
                   (if (null optional2)
                       ;; required v rest
                       (push (disjoin/2 client (pop required1) rest2)
                             optional)
                       ;; required v optional
                       (push (disjoin/2 client (pop required1) (pop optional2))
                             optional))
                   ;; required v required
                   (push (disjoin/2 client (pop required1) (pop required2))
                         required)))
        when donep
          return (values client (nreverse required) (nreverse optional) rest)))

(defmethod values-wdisjoin/2 (client vct1 vct2)
  ;; FIXME: This is not actually Noetherian right now, since more values can
  ;; get tacked on indefinitely!
  (assert (and (values-ctype-p vct1) (values-ctype-p vct2)))
  (cond ((values-bottom-p client vct1)
         (return-from values-wdisjoin/2 vct2))
        ((values-bottom-p client vct2)
         (return-from values-wdisjoin/2 vct1)))
  ;; General case
  (loop with required1 = (values-required client vct1)
        with optional1 = (values-optional client vct1)
        with rest1 = (values-rest client vct1)
        with required2 = (values-required client vct2)
        with optional2 = (values-optional client vct2)
        with rest2 = (values-rest client vct2)
        with required with optional with rest
        with donep = nil
        do (if (null required1)
               (if (null optional1)
                   (if (null required2)
                       (if (null optional2)
                           ;; rest v rest
                           (setf rest (wdisjoin/2 client rest1 rest2)
                                 donep t)
                           ;; rest v opt
                           (push (wdisjoin/2 client rest1 (pop optional2))
                                 optional))
                       ;; rest v req
                       (push (wdisjoin/2 client rest1 (pop required2))
                             optional))
                   (if (null required2)
                       (if (null optional2)
                           ;; optional v rest
                           (push (wdisjoin/2 client (pop optional1) rest2)
                                 optional)
                           ;; optional v optional
                           (push (wdisjoin/2 client (pop optional1) (pop optional2))
                                 optional))
                       ;; optional v req
                       (push (wdisjoin/2 client (pop optional1) (pop required2))
                             optional)))
               (if (null required2)
                   (if (null optional2)
                       ;; required v rest
                       (push (wdisjoin/2 client (pop required1) rest2)
                             optional)
                       ;; required v optional
                       (push (wdisjoin/2 client (pop required1) (pop optional2))
                             optional))
                   ;; required v required
                   (push (wdisjoin/2 client (pop required1) (pop required2))
                         required)))
        when donep
          return (values client (nreverse required) (nreverse optional) rest)))

(defmethod disjoin/2 (client ct1 ct2)
  (cond
    ((or (values-ctype-p ct1) (values-ctype-p ct2))
     (error "values ctypes ~a ~a input to disjoin" ct1 ct2))
    ((top-p client ct1) ct1)
    ((top-p client ct2) ct2)
    ((bottom-p client ct1) ct2)
    ((bottom-p client ct2) ct1)
    ((cl:subtypep ct1 ct2) ct2)
    ((cl:subtypep ct2 ct1) ct1)
    (t `(or ,ct1 ,ct2))))

(defmethod wdisjoin/2 (client ct1 ct2)
  (cond
    ((top-p client ct1) ct1)
    ((top-p client ct2) ct2)
    ((bottom-p client ct1) ct2)
    ((bottom-p client ct2) ct1)
    (t (let ((sum `(or ,ct1 ,ct2)))
         (macrolet ((tcases (&rest type-specifiers)
                      `(cond
                         ,@(loop for ts in type-specifiers
                                 collect `((cl:subtypep sum ',ts) ',ts))
                         (t (top client)))))
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

(defmethod negate (client ct)
  (cond ((top-p client ct) 'nil)
        ((bottom-p client ct) 't)
        (t `(not ,ct))))

(defmethod subtract (client ct1 ct2)
  (cond ((bottom-p client ct1) 'nil)
        ((bottom-p client ct2) ct1)
        ((top-p client ct2) 'nil)
        (t (conjoin/2 client ct1 (negate client ct2)))))

(defmethod values-append/2 (client ct1 ct2)
  ;; This is considerably complicated by nontrivial &optional and &rest.
  ;; For a start (to be improved? FIXME) we take the required values of the
  ;; first form, and record the minimum number of required values, which is
  ;; just the sum of those of the values types.
  ;; Also, if the number of values of the first type is fixed (no &optional
  ;; and the &rest is bottom) we give the simple exact result.
  (let ((req1 (values-required client ct1))
        (opt1 (values-optional client ct1))
        (rest1 (values-rest client ct1))
        (req2 (values-required client ct2))
        (opt2 (values-optional client ct2))
        (rest2 (values-rest client ct2)))
    (if (and (null opt1) (bottom-p client rest1))
        ;; simple case
        (values client (append req1 req2) opt2 rest2)
        ;; Approximate as described
        (values
         client
         (append req1 (make-list (length req2)
                                 :initial-element (top client)))
         nil (top client)))))

(defun function-returns (fctype) (third fctype))

(defun general-function-returns (client fctype)
  (cond ((function-ctype-p fctype)
         (function-returns fctype))
        ((conjunctionp client fctype)
         (cl:apply #'conjoin client
                   (loop for fc in (conjunction-ctypes client fctype)
                         collect (general-function-returns client fc))))
        ((disjunctionp client fctype)
         (cl:apply #'disjoin client
                   (loop for fc in (disjunction-ctypes client fctype)
                         collect (general-function-returns client fc))))
        ;; give up
        (t `(cl:values &rest t))))

(defmethod apply (client fctype actype)
  (declare (ignore actype))
  (general-function-returns client fctype))

(defmethod funcall (client fctype &rest atypes)
  (declare (ignore atypes))
  (general-function-returns client fctype))

(defmethod class (client class) (declare (ignore client)) class)

(defmethod cons (client car cdr)
  (declare (ignore client))
  (cond ((eql car 'nil) 'nil)
        ((eql cdr 'nil) 'nil)
        (t `(cl:cons ,car ,cdr))))

(defmethod consp (client type)
  (declare (ignore client))
  (and (cl:consp type) (eq (car type) 'cl:cons)
       (cl:consp (cdr type))
       (cl:consp (cddr type))
       (cl:null (cdddr type))))
(defmethod cons-car (client type)
  (declare (ignore client))
  (second type))
(defmethod cons-cdr (client type)
  (declare (ignore client))
  (third type))

(defun normalize-dimensions (dimensions)
  (if (integerp dimensions)
      (make-list dimensions :initial-element '*)
      dimensions))
(defmethod array (client element dimensions simplicity)
  (declare (ignore client))
  `(,simplicity ,element ,dimensions))
(defmethod arrayp (client type)
  ;; FIXME: Doesn't work on string types.
  (declare (ignore client))
  (and (cl:consp type) (cl:member (car type) '(cl:array cl:simple-array))
       (cl:consp (cdr type))
       (cl:consp (cddr type))
       (cl:null (cdddr type))))
(defmethod array-element-type (client type)
  (declare (ignore client))
  (second type))
(defmethod array-dimensions (client type)
  (declare (ignore client))
  (third type))

(defmethod string (client dimension simplicity)
  (declare (ignore client))
  `(,(ecase simplicity
       ((cl:array) 'cl:string)
       ((cl:simple-array) 'cl:simple-string))
    ,dimension))

(defmethod character (client) (declare (ignore client)) 'cl:character)
(defmethod base-char (client) (declare (ignore client)) 'cl:base-char)
(defmethod standard-char (client) (declare (ignore client)) 'cl:standard-char)

(defmethod complex (client part)
  (declare (ignore client))
  `(cl:complex ,part))

(defmethod complexp (client type)
  (declare (ignore client))
  (and (cl:consp type) (eq (car type) 'cl:complex)
       (cl:consp (cdr type)) (cl:null (cddr type))))
(defmethod complex-part-type (client type)
  (declare (ignore client))
  (second type))

(defmethod range (client type low high)
  (declare (ignore client))
  `(,type ,low ,high))
(defmethod rangep (client type)
  (declare (ignore client))
  (and (cl:consp type) (cl:member (car type) '(cl:integer cl:ratio cl:rational
                                               cl:float cl:single-float cl:double-float
                                               cl:short-float cl:long-float cl:real))
       (cl:consp (cdr type))
       (cl:consp (cddr type)) (cl:null (cdddr type))))
(defun process-interval-designator (desig)
  (cond ((eq desig '*) (cl:values nil nil))
        ((listp desig) (cl:values (first desig) t))
        (t (cl:values desig nil))))
(defmethod range-kind (client type)
  (declare (ignore client))
  (first type))
(defmethod range-low (client type)
  (declare (ignore client))
  (process-interval-designator (second type)))
(defmethod range-high (client type)
  (declare (ignore client))
  (process-interval-designator (third type)))

(defmethod fixnum (client) (declare (ignore client)) 'cl:fixnum)

(defun constant-real->range (client real)
  (range
   client
   (etypecase real
     ((integer) 'integer)
     ((rational) 'rational)
     ((single-float) 'single-float)
     ((double-float) 'double-float)
     ((short-float) 'short-float)
     ((long-float) 'long-float))
   real real))

(defmethod member (client &rest elems)
  ;; Try to represent reals as ranges instead.
  (let ((reals (remove-if-not #'realp elems)))
    (if reals
        (cl:apply #'disjoin client `(cl:member ,@(set-difference elems reals))
                  (loop for real in reals
                        collecting (constant-real->range client real)))
        `(cl:member ,@elems))))

(defmethod member-p (client ctype)
  (declare (ignore client))
  (and (cl:consp ctype) (eq (first ctype) 'cl:member)))
(defmethod member-members (client ctype)
  (declare (ignore client))
  (rest ctype))

(defmethod satisfies (client fname)
  (declare (ignore client))
  `(cl:satisfies ,fname))

(defmethod keyword (client) (declare (ignore client)) 'cl:keyword)

(defmethod function (client req opt rest keyp keys aokp returns)
  (declare (ignore client))
  `(cl:function (,@req &optional ,@opt &rest ,rest
                       ,@(when keyp `(&key ,@keys))
                       ,@(when aokp '(&allow-other-keys)))
                ,returns))

(defmethod compiled-function (client) (declare (ignore client))
  'cl:compiled-function)

(defmethod values (client req opt rest)
  (declare (ignore client))
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

(defmethod values-required (client ctype)
  (declare (ignore client))
  (ll-required (cl:rest ctype)))

(defmethod values-optional (client ctype)
  (declare (ignore client))
  (ll-optional (cl:rest ctype)))

(defmethod values-rest (client ctype)
  (declare (ignore client))
  (ll-rest (cl:rest ctype)))

(defmethod nth-value (client n ctype)
  (let* ((req (values-required client ctype))
         (nreq (length req)))
    (cond ((< n nreq) (nth n req))
          ((some (lambda (ct) (bottom-p client ct)) req) (bottom client))
          (t (disjoin
              client
              (member client nil)
              (let* ((opt (values-optional client ctype))
                     (nopt (length opt)))
                (if (< n (+ nreq nopt))
                    (nth (- n nreq) opt)
                    (values-rest client ctype))))))))

(defmethod function-required (client ctype)
  (declare (ignore client))
  (ll-required (second ctype)))

(defmethod function-optional (client ctype)
  (declare (ignore client))
  (ll-optional (second ctype)))

(defmethod function-rest (client ctype)
  (declare (ignore client))
  (ll-rest (second ctype)))

(defmethod function-keysp (client ctype)
  (declare (ignore client))
  (ll-keysp (second ctype)))

(defmethod function-keys (client ctype)
  (declare (ignore client))
  (ll-keys (second ctype)))

(defmethod function-allow-other-keys-p (client ctype)
  (declare (ignore client))
  (ll-aokp (second ctype)))

(defmethod function-values (client ctype)
  (declare (ignore client))
  (third ctype))
