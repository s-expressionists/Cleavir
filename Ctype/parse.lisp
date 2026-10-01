(in-package #:cleavir-ctype)

(define-condition no-parse (error)
  ((%client :initarg :client :reader client)
   (%specifier :initarg :specifier :reader specifier))
  (:report (lambda (condition stream)
             (format stream "Don't know how to parse type specifier~%~t~s~%(with client ~s)"
                     (specifier condition) (client condition)))))

(defgeneric parse (client specifier)
  (:method (client specifier)
    (error 'no-parse :client client :specifier specifier)))

(defun approximate-parse (client specifier)
  (handler-case (parse client specifier)
    (no-parse () (cl:values nil nil))
    (:no-error (ctype) (cl:values ctype t))))

(defmethod parse (client (specifier (eql 'cl:array)))
  (array '* '* 'cl:array client))

(defmethod parse (client (specifier (eql 'atom)))
  (negate (cons (top client) (top client) client) client))

(defmethod parse (client (specifier (eql 'cl:base-char)))
  (base-char client))

(defmethod parse (client (specifier (eql 'base-string)))
  (array (base-char client) '(*) 'cl:array client))

(defmethod parse (client (specifier (eql 'bignum)))
  (conjoin client
           (range 'integer '* '* client)
           (negate (fixnum client) client)))

(defmethod parse (client (specifier (eql 'bit)))
  (range 'integer 0 1 client))

(defmethod parse (client (specifier (eql 'bit-vector)))
  (array (upgraded-array-element-type (range 'integer 0 1 client) client)
         '(*) 'cl:array client))

(defmethod parse (client (specifier (eql 'cl:character))) (character client))

(defmethod parse (client (specifier (eql 'cl:compiled-function)))
  (compiled-function client))

(defmethod parse (client (specifier (eql 'cl:complex))) (complex '* client))

(defmethod parse (client (specifier (eql 'cons)))
  (cons (top client) (top client) client))

(defmethod parse (client (specifier (eql 'double-float)))
  (range 'double-float '* '* client))

(defmethod parse (client (specifier (eql 'cl:fixnum))) (fixnum client))

(defmethod parse (client (specifier (eql 'float))) (range 'float '* '* client))

(defmethod parse (client (specifier (eql 'cl:function))) (function-top client))

(defmethod parse (client (specifier (eql 'integer))) (range 'integer '* '* client))

(defmethod parse (client (specifier (eql 'cl:keyword))) (keyword client))

(defmethod parse (client (specifier (eql 'list)))
  (disjoin client (member client nil) (cons (top client) (top client) client)))

(defmethod parse (client (specifier (eql 'long-float)))
  (range 'long-float '* '* client))

(defmethod parse (client (specifier (eql 'nil))) (bottom client))

(defmethod parse (client (specifier (eql 'null))) (member client nil))

(defmethod parse (client (specifier (eql 'number)))
  (disjoin client (range 'real '* '* client) (complex '* client)))

(defmethod parse (client (specifier (eql 'rational)))
  (range 'rational '* '* client))

(defmethod parse (client (specifier (eql 'real))) (range 'real '* '* client))

(defmethod parse (client (specifier (eql 'short-float)))
  (range 'short-float '* '* client))

(defmethod parse (client (specifier (eql 'signed-byte)))
  (range 'integer '* '* client))

(defmethod parse (client (specifier (eql 'simple-array)))
  (array '* '* 'simple-array client))

(defmethod parse (client (specifier (eql 'simple-base-string)))
  (array (upgraded-array-element-type (base-char client) client)
         '(*) 'simple-array client))

(defmethod parse (client (specifier (eql 'simple-bit-vector)))
  (array (upgraded-array-element-type (range 'integer 0 1 client) client)
         '(*) 'simple-array client))

(defmethod parse (client (specifier (eql 'simple-string)))
  (string '* 'simple-array client))

(defmethod parse (client (specifier (eql 'simple-vector)))
  (array (upgraded-array-element-type (top client) client)
         '(*) 'simple-array client))

(defmethod parse (client (specifier (eql 'single-float)))
  (range 'single-float '* '* client))

(defmethod parse (client (specifier (eql 'standard-char))) (standard-char client))

(defmethod parse (client (specifier (eql 'string))) (string '* 'array client))

(defmethod parse (client (specifier (eql 't))) (top client))

(defmethod parse (client (specifier (eql 'unsigned-byte)))
  (range 'integer 0 '* client))

(defmethod parse (client (specifier (eql 'vector)))
  (array '* '(*) 'array client))

(defmethod parse (client (specifier cl:class)) (class specifier client))

(defmethod parse (client (specifier cl:cons))
  (parse-compound client (car specifier) (cdr specifier)))

(defgeneric parse-compound (client specifier arguments)
  (:method (client specifier arguments)
    (error 'no-parse :client client :specifier (list* specifier arguments))))

(defmethod parse-compound (client (spec (eql 'and)) arguments)
  (cl:apply #'conjoin client
            (loop for arg in arguments collect (parse client arg))))

(defun validate-dimension (dim)
  (cond ((eq dim '*) dim)
        ((and (integerp dim) (>= dim 0)) dim)
        (t (error "Invalid dimension: ~s" dim))))

(defun validate-dimensions (dims)
  (cond ((eq dims '*) dims)
        ((and (integerp dims) (>= dims 0))
         (make-list dims :initial-element '*))
        (t
         (unless (loop with d = dims
                       until (null d)
                       always (and (cl:consp d)
                                   (or (eq (car d) '*)
                                       (and (integerp (car d))
                                            (>= (car d) 0)))))
           (error "Invalid dimensions: ~s" dims))
         dims)))

(defmethod parse-compound (client (spec (eql 'cl:array)) arguments)
  (destructuring-bind (&optional (et '*) (dims '*)) arguments
    (let ((uaet (if (eq et '*)
                    et
                    (upgraded-array-element-type (parse client et) client)))
          (dims (validate-dimensions dims)))
      (array uaet dims 'array client))))

(defmethod parse-compound (client (spec (eql 'base-string)) arguments)
  (destructuring-bind (&optional (dim '*)) arguments
    (array (upgraded-array-element-type (base-char client) client)
           (list (validate-dimension dim)) 'array client)))

(defmethod parse-compound (client (spec (eql 'bit-vector)) arguments)
  (destructuring-bind (&optional (dim '*)) arguments
    (array (upgraded-array-element-type (range 'integer 0 1 client) client)
           (list (validate-dimension dim)) 'array client)))

(defmethod parse-compound (client (spec (eql 'cl:complex)) arguments)
  (destructuring-bind (&optional (part '*)) arguments
    (complex (upgraded-complex-part-type (parse client part) client) client)))

(defmethod parse-compound (client (spec (eql 'cl:cons)) arguments)
  (destructuring-bind (&optional (car '*) (cdr '*)) arguments
    (cons (if (eq car '*) (top client) (parse client car))
          (if (eq cdr '*) (top client) (parse client cdr))
          client)))

(defun check-bounds (type low high)
  (unless (or (eq low '*) (typep low type))
    (error "Invalid ~s low bound: ~s" type low))
  (unless (or (eq high '*) (typep high type))
    (error "Invalid ~s high bound: ~s" type high)))

(defun parse-range (client spec arguments)
  (destructuring-bind (&optional (low '*) (high '*)) arguments
    (check-bounds spec low high)
    (range spec low high client)))

(defmethod parse-compound (client (spec (eql 'double-float)) arguments)
  (parse-range client spec arguments))

(defmethod parse-compound (client (spec (eql 'eql)) arguments)
  (destructuring-bind (object) arguments
    (member client object)))

(defmethod parse-compound (client (spec (eql 'float)) arguments)
  (parse-range client spec arguments))

(defmethod parse-compound (client (spec (eql 'cl:function)) arguments)
  (declare (ignore client arguments))
  (error "TODO"))

(defmethod parse-compound (client (spec (eql 'integer)) arguments)
  (parse-range client spec arguments))

(defmethod parse-compound (client (spec (eql 'long-float)) arguments)
  (parse-range client spec arguments))

(defmethod parse-compound (client (spec (eql 'cl:member)) arguments)
  (apply #'member client arguments))

(defmethod parse-compound (client (spec (eql 'mod)) arguments)
  (destructuring-bind (modulus) arguments
    (unless (and (integerp modulus) (> modulus 0))
      (error "Invalid modulus: ~s" modulus))
    (range 'integer 0 (1- modulus) client)))

(defmethod parse-compound (client (spec (eql 'not)) arguments)
  (destructuring-bind (neg) arguments
    (negate (parse client neg) client)))

(defmethod parse-compound (client (spec (eql 'or)) arguments)
  (cl:apply #'disjoin client
            (loop for st in arguments collect (parse client st))))

(defmethod parse-compound (client (spec (eql 'rational)) arguments)
  (parse-range client spec arguments))

(defmethod parse-compound (client (spec (eql 'real)) arguments)
  (parse-range client spec arguments))

(defmethod parse-compound (client (spec (eql 'cl:satisfies)) arguments)
  (destructuring-bind (predicate) arguments
    (unless (symbolp predicate) (error "Invalid ~s predicate: ~s" spec predicate))
    (satisfies predicate client)))

(defmethod parse-compound (client (spec (eql 'short-float)) arguments)
  (parse-range client spec arguments))

(defmethod parse-compound (client (spec (eql 'signed-byte)) arguments)
  (destructuring-bind (&optional (nbits '*)) arguments
    (cond ((eq nbits '*) (range 'integer '* '* client))
          ((and (integerp nbits) (>= nbits 0))
           (range 'integer (- (ash 1 (1- nbits))) (1- (ash 1 (1- nbits))) client))
          (t (error "Invalid ~s bit count: ~s" spec nbits)))))

(defmethod parse-compound (client (spec (eql 'simple-array)) arguments)
  (destructuring-bind (&optional (et '*) (dims '*)) arguments
    (let ((uaet (if (eq et '*)
                    et
                    (upgraded-array-element-type (parse client et) client)))
          (dims (validate-dimensions dims)))
      (array uaet dims 'simple-array client))))

(defmethod parse-compound (client (spec (eql 'simple-base-string)) arguments)
  (destructuring-bind (&optional (dim '*)) arguments
    (array (upgraded-array-element-type (base-char client) client)
           (list (validate-dimension dim)) 'simple-array client)))

(defmethod parse-compound (client (spec (eql 'simple-bit-vector)) arguments)
  (destructuring-bind (&optional (dim '*)) arguments
    (array (upgraded-array-element-type (range 'integer 0 1 client) client)
           (list (validate-dimension dim)) 'simple-array client)))

(defmethod parse-compound (client (spec (eql 'simple-string)) arguments)
  (destructuring-bind (&optional (dim '*)) arguments
    (string (validate-dimension dim) 'simple-array client)))

(defmethod parse-compound (client (spec (eql 'simple-vector)) arguments)
  (destructuring-bind (&optional (dim '*)) arguments
    (array (upgraded-array-element-type (top client) client)
           (list (validate-dimension dim)) 'simple-array client)))

(defmethod parse-compound (client (spec (eql 'single-float)) arguments)
  (parse-range client spec arguments))

(defmethod parse-compound (client (spec (eql 'string)) arguments)
  (destructuring-bind (&optional (dim '*)) arguments
    (string (validate-dimension dim) 'array client)))

(defmethod parse-compound (client (spec (eql 'unsigned-byte)) arguments)
  (destructuring-bind (&optional (nbits '*)) arguments
    (cond ((eq nbits '*) (range 'integer '* '* client))
          ((and (integerp nbits) (>= nbits 0))
           (range 'integer 0 (1- (ash 1 nbits)) client))
          (t (error "Invalid ~s bit count: ~s" spec nbits)))))

(defmethod parse-compound (client (spec (eql 'cl:values)) arguments)
  (declare (ignore client arguments))
  (error "TODO"))

(defmethod parse-compound (client (spec (eql 'vector)) arguments)
  (destructuring-bind (&optional (et '*) (dim '*)) arguments
    (let ((uaet (if (eq et '*)
                    et
                    (upgraded-array-element-type (parse client et) client)))
          (dims (list (validate-dimension dim))))
      (array uaet dims 'array client))))
