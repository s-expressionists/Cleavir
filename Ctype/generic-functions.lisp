(in-package #:cleavir-ctype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions SUBTYPEP, UPGRADED-ARRAY-ELEMENT-TYPE,
;;; and UPGRADED-COMPLEX-PART-TYPE.
;;;
;;; As in CL, but with ctypes, no environment, and a client
;;; parameter. SUBTYPEP may only be called with two non-values ctypes
;;; or two values ctypes. U-A-E-T and U-C-P-T may not be called with
;;; values ctypes.

(defgeneric subtypep (ctype1 ctype2 system))

(defgeneric upgraded-array-element-type (ctype system))
(defgeneric upgraded-complex-part-type (ctype system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SUBTYPEP.
;;;
;;; This version of SUBTYPEP works with values ctypes as well.
(defgeneric values-subtypep (ctype1 ctype2 system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions TOP, BOTTOM.
;;;
;;; Return top or bottom ctypes (i.e. ctypes of T and NIL respectively).
;;; Required to do even very basic type operations.

(defgeneric top (system))
(defgeneric bottom (system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions VALUES-TOP, VALUES-BOTTOM.
;;;
;;; Return top or bottom values ctypes. The top values ctype is equivalent to
;;; (values &rest t), representing any number of values, each of which may be
;;; of any type. The bottom values ctype is equivalent to (values nil &rest nil)
;;; and represents no number of values - which is, importantly, a distinct type
;;; from (values &rest nil), meaning exactly zero values. That is to say, there
;;; is no list of values that is of the bottom values ctype.
;;; This long comment is because this is genuinely confusing.

(defgeneric values-top (system))
(defgeneric values-bottom (system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions TOP-P, BOTTOM-P.
;;;
;;; Return whether the given ctype is the top or bottom ctype respectively.
;;; These functions are intended to be quick rather than necessarily correct
;;; (which is uncomputable in the presence of SATISFIES anyway). If they return
;;; true, that must be accurate, but they are permitted to return false even if
;;; the ctype actually is top or bottom respectively.
;;;
;;; Neither function may be passed a values ctype.

(defgeneric top-p (ctype system))
(defgeneric bottom-p (ctype system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions CONJOIN/2, DISJOIN/2.
;;;
;;; Given two non-values ctypes, compute their conjunction or disjunction, as
;;; for the AND and OR type specifiers respectively.
;;;
;;; Called by the n-ary CONJOIN and DISJOIN.

(defgeneric conjoin/2 (ctype1 ctype2 system))
(defgeneric disjoin/2 (ctype1 ctype2 system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions VALUES-CONJOIN, VALUES-DISJOIN.
;;;
;;; Given two values ctypes, compute their conjunction or disjunction.

(defgeneric values-conjoin (ctype1 ctype2 system))
(defgeneric values-disjoin (ctype1 ctype2 system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NEGATE.
;;;
;;; Compute the negation of the given ctype, as if using the NOT type specifier.
;;; May not be called with a values ctype.

(defgeneric negate (ctype system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SUBTRACT.
;;;
;;; Compute the difference of the two ctypes.
;;; (subtract c1 c2 s) = (conjoin/2 c1 (negate c2 s) s), but this can sometimes
;;; be done more efficiently. May not be called with values ctypes.

(defgeneric subtract (ctype1 ctype2 system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function APPLY.
;;;
;;; Given a ctype of a function, and the ctype of a list of arguments,
;;; return a ctype for the return values from CL:APPLYing the function to those
;;; arguments.

(defgeneric apply (fctype actype system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCALL.
;;;
;;; Given a ctype of a function, and a list of argument ctypes,
;;; return a ctype for the return values from CL:APPLYing the function to those
;;; arguments.

(defgeneric funcall (system fctype &rest actypes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CLASS.
;;;
;;; Given a class, return the ctype for that class.

(defgeneric class (class system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CONS.
;;;
;;; Given two non-values ctypes, return the ctype of a cons type using them.

(defgeneric cons (car cdr system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ARRAY.
;;;
;;; Given an element type specifier, dimensions specifier, and simplicity mark,
;;; return a ctype representing the array type.
;;; The element type specifier is either an upgraded non-values ctype,
;;; or the symbol *.
;;; The dimensions specifier is as in the CL type specifier ARRAY.
;;; The simplicity mark is one of the symbols ARRAY or SIMPLE-ARRAY.

(defgeneric array (element dimensions simplicity system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function STRING.
;;;
;;; Given a dimension specifier, and simplicity mark, return a ctype
;;; representing the string type.
;;; The dimension specifier is either a nonnegative integer or the symbol *.
;;; The simplicity mark is one of the symbols ARRAY or SIMPLE-ARRAY.

(defgeneric string (dimension simplicity system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CHARACTER.
;;;
;;; Return the ctype representing CL:CHARACTER.

(defgeneric character (system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BASE-CHAR.
;;;
;;; Return the ctype representing CL:BASE-CHAR.

(defgeneric base-char (system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function STANDARD-CHAR.
;;;
;;; Return the ctype representing CL:STANDARD-CHAR.

(defgeneric standard-char (system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMPLEX.
;;;
;;; Given a part type specifier, return a ctype for the complex type.
;;; The specifier may be either an upgraded non-values ctype or the symbol *.

(defgeneric complex (part system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function RANGE.
;;;
;;; Given a type name and interval designators, return a ctype.
;;; The type name is one of the symbols INTEGER, RATIONAL, REAL, FLOAT,
;;; SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, or LONG-FLOAT.

(defgeneric range (type low high system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FIXNUM.
;;;
;;; Return the ctype for CL:FIXNUM.

(defgeneric fixnum (system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MEMBER.

(defgeneric member (system &rest elements))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions MEMBER-P, MEMBER-MEMBERS.
;;;
;;; Return information about a non-values ctype.
;;; It is undefined behavior if MEMBER-MEMBERS is called on a ctype which
;;; MEMBER-P is not true of.

(defgeneric member-p (system non-values-ctype))
(defgeneric member-members (system member-ctype))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SATISFIES.

(defgeneric satisfies (fname system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function KEYWORD.
;;;
;;; Returns the ctype representing CL:KEYWORD.
;;; While KEYWORD = (SATISFIES KEYWORD), this function exists in case a client
;;; has a more specialized representation, for example to make clear that it is
;;; a subtype of SYMBOL.

(defgeneric keyword (system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION.
;;;
;;; Return a ctype for a function type specifier.
;;;
;;; The first seven parameters represent the parsed lambda list.
;;; REQUIRED and OPTIONAL are lists of non-values ctypes, and REST is a
;;; non-values ctype. KEYP and ALLOW-OTHER-KEYS-P indicate the presences of
;;; &key and &allow-other-keys respectively. KEYS is a list of
;;; (keyword non-values-ctype) elements.
;;; RETURNS is the values ctype of the return values specified.
;;;
;;; Note that REST must always be provided. If the function does not actually
;;; accept &rest arguments, this should be indicated by REST being the bottom
;;; ctype.

(defgeneric function
    (required optional rest keyp keys allow-other-keys-p returns system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMPILED-FUNCTION.
;;;
;;; Returns the ctype for CL:COMPILED-FUNCTION.
;;; While COMPILED-FUNCTION = (SATISFIES COMPILED-FUNCTION-P), this function
;;; exists in case a client has a more specialized representation, for example
;;; to make clear that it is a subtype of FUNCTION.

(defgeneric compiled-function (system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VALUES.
;;;
;;; Return a ctype for a values type specifier.
;;;
;;; The first four parameters represent the parsed lambda list.
;;; REQUIRED and OPTIONAL are lists of ctypes. REST is a ctype.
;;;
;;; Note that REST must always be provided. To match the semantics of CL:THE, a
;;; values type specifier with no &rest may be considered to have an implicit
;;; &rest T, and that T ctype is expected to be provided by whatever code calls
;;; this function.
;;;
;;; The semantics of values types in the standard are self contradictory. For
;;; this system, the strict semantics described in the page on VALUES are
;;; ignored as being impractical. The semantics for CL:THE are used instead.
;;; So for example, (values 0 '(x)), (values 4), (values 23 nil 'values) are
;;; all valid forms in (the (values integer list) form), but (values) wouldn't
;;; be. This values type would be passed to this function with a REST that is
;;; the ctype for T.
;;;
;;; Values ctypes are only used in certain contexts, as described for the
;;; operators above.

(defgeneric values (required optional rest system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COERCE-TO-VALUES.
;;;
;;; Given a ctype, return a values ctype. In more detail, if the ctype is a
;;; values ctype, it is returned; otherwise, a values ctype for an equivalent to
;;; (values type &rest t) is returned, thus incorporating the fuzziness.

(defgeneric coerce-to-values (ctype system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions VALUES-REQUIRED, VALUES-OPTIONAL, VALUES-REST.
;;; Read components of a values ctype.

(defgeneric values-required (ctype system))
(defgeneric values-optional (ctype system))
(defgeneric values-rest (ctype system))

(defgeneric nth-value (n ctype system)
  (:argument-precedence-order ctype system n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions FUNCTION-REQUIRED, FUNCTION-OPTIONAL, FUNCTION-REST, FUNCTION-KEYSP, FUNCTION-KEYS, FUNCTION-ALLOW-OTHER-KEYS-P,
;;; and FUNCTION-VALUES.
;;;
;;; Read components of a function ctype.

(defgeneric function-required (ctype system))
(defgeneric function-optional (ctype system))
(defgeneric function-rest (ctype system))
(defgeneric function-keysp (ctype system))
(defgeneric function-keys (ctype system))
(defgeneric function-allow-other-keys-p (ctype system))
(defgeneric function-values (ctype system))
