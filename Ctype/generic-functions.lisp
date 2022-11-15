(in-package #:cleavir-ctype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions SUBTYPEP, UPGRADED-ARRAY-ELEMENT-TYPE,
;;; and UPGRADED-COMPLEX-PART-TYPE.
;;;

(defgeneric subtypep (ctype1 ctype2 system)
  (:documentation "As CL:SUBTYPEP, but with non-values ctypes instead of type specifiers, no tnevironment, and a client parameter.

See VALUES-SUBTYPEP"))

(defgeneric upgraded-array-element-type (ctype system)
  (:documentation "As CL:UPGRADED-ARRAY-ELEMENT-TYPE, but with a non-values ctype instead of a type specifier, and a client parameter instead of an environment."))
(defgeneric upgraded-complex-part-type (ctype system)
  (:documentation "As CL:UPGRADED-COMPLEX-PART-TYPE, but with a non-values ctype instead of a type specifier, and a client parameter instead of an environment."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SUBTYPEP.
;;;
(defgeneric values-subtypep (ctype1 ctype2 system)
  (:documentation "SUBTYPEP for values ctypes. Non-values ctypes are not permitted as arguments.

See SUBTYPEP"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions TOP, BOTTOM.
;;;

(defgeneric top (system)
  (:documentation "Return the top ctype, i.e. the ctype for CL:T.

See VALUES-TOP"))
(defgeneric bottom (system)
  (:documentation "Return the bottom ctype, i.e. the ctype for CL:NIL.

See VALUES-BOTTOM"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions VALUES-TOP, VALUES-BOTTOM.
;;;

(defgeneric values-top (system)
  (:documentation "Return the top values ctype. The top values ctype is equivalent to (values &rest t), representing any number of values, each of which may be of any type.

See TOP"))
(defgeneric values-bottom (system)
  (:documentation "Return the bottom values ctype. The bottom values ctype is equivalent to (values nil &rest nil) and represents no number of values - which is, importantly, a distinct type from (values &rest nil), which means exactly zero values. That is to say, there is no list of values that is of the bottom values ctype.

See BOTTOM"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions TOP-P, BOTTOM-P.
;;;

(defgeneric top-p (ctype system)
  (:documentation "Return true if the given non-values ctype is the top ctype (i.e. CL:T). As this function is approximate, false may be returned even on a ctype equivalent to top. (For example, it may not be possible to determine this equivalence in the presence of SATISFIES types.)"))
(defgeneric bottom-p (ctype system)
  (:documentation "Return true if the given non-values ctype is the bottom ctype (i.e. CL:NIL). As this function is approximate, false may be returned even on a ctype equivalent to bottom. (For example, it may not be possible to determine this equivalence in the presence of SATISFIES types.)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions CONJOIN/2, DISJOIN/2.
;;;

(defgeneric conjoin/2 (ctype1 ctype2 system)
  (:documentation "Return the conjunction (i.e. CL:AND) of two non-values ctypes.
This function may be specialized, but should not be called directly: use CONJOIN.

See CONJOIN"))
(defgeneric disjoin/2 (ctype1 ctype2 system)
  (:documentation "Return the disjunction (i.e. CL:OR) of two non-values ctypes.
This function may be specialized, but should not be called directly: use DISJOIN.

See DISJOIN"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions CONJUNCTIONP, DISJUNCTIONP.
;;;

(defgeneric conjunctionp (ctype system)
  (:documentation "Return true iff the given non-values ctype is a conjunction ctype.
Note that whether a given type specifier ends up as a conjunction ctype may be client-dependent. For example, (and (integer 1 7) (integer 2 4)) may end up as a conjunction on some clients, but reduced to an integer type by others."))
(defgeneric disjunctionp (ctype system)
  (:documentation "Return true iff the given non-values ctype is a disjunction ctype.
Note that whether a given type specifier ends up as a disjunction ctype may be client dependent. For example, (or (integer 1 7) (integer 4 9)) may end up as a disjunction on some clients, but reduced to (integer 1 9) on others."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions CONJUNCTION-CTYPES, DISJUNCTION-CTYPES.
;;;

(defgeneric conjunction-ctypes (conjunction-ctype system)
  (:documentation "Given a conjunction ctype, return a list of the ctypes it is composed of, in some arbitrary order. This function must only be called on ctypes that CONJUNCTIONP is true of.

See CONJUNCTIONP"))
(defgeneric disjunction-ctypes (disjunction-ctype system)
  (:documentation "Given a disjunction ctype, return a list of the ctypes it is composed of, in some arbitrary order. This function must only be called on ctypes that DISJUNCTIONP is true of.

See DISJUNCTIONP"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function WDISJOIN/2.
;;;
;;; Widening disjunction: As DISJOIN/2, but the result lattice must be
;;; Noetherian, i.e. any sequence of WDISJOIN operations must be
;;; eventually stationary.
;;;
;;; Called by the n-ary WDISJOIN.

(defgeneric wdisjoin/2 (ctype1 ctype2 system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions VALUES-CONJOIN/2, VALUES-DISJOIN/2.
;;;

(defgeneric values-conjoin/2 (ctype1 ctype2 system)
  (:documentation "Given two values ctypes, return their conjunction.
This function may be specialized, but should not be called directly: Use VALUES-CONJOIN.

See VALUES-CONJOIN"))
(defgeneric values-disjoin/2 (ctype1 ctype2 system)
  (:documentation "Given two values ctypes, return their disjunction.
This function may be specialized, but should not be called directly: Use VALUES-DISJOIN.

See VALUES-DISJOIN"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VALUES-WDISJOIN/2.
;;;
;;; Widening values disjunction, analogous to VALUES-DISJOIN/2 using
;;; WDISJOIN/2.
;;;
;;; Called by the n-ary VALUES-WDISJOIN.

(defgeneric values-wdisjoin/2 (ctype1 ctype2 system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VALUES-APPEND/2.
;;;
;;; FIXME: This cannot always be exactly expressed as a values type, e.g.
;;; (values-append '(values &optional a) '(values b)) is not
;;; (values &optional a b) which would allow no values. Further thinking may
;;; be required.

(defgeneric values-append/2 (ctype1 ctype2 system)
  (:documentation "Given two values ctypes, append them.
In more detail, if forms A and B have types Avt and Bvt, (values-append Avt Bvt) is the type of (multiple-value-call #'values A B). This operation is useful when dealing with multiple-value-call.
This function may be specialized, but should not be called directly: Use VALUES-APPEND.

See VALUES-APPEND"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NEGATE.
;;;

(defgeneric negate (ctype system)
  (:documentation "Compute the negation of the given non-values ctype, as if using the NOT type specifier."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SUBTRACT.
;;;

(defgeneric subtract (ctype1 ctype2 system)
  (:documentation "Compute the difference of the two non-values ctypes.
;;; (subtract c1 c2 s) = (conjoin/2 c1 (negate c2 s) s), but this can sometimes
;;; be done more efficiently. May not be called with values ctypes."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function APPLY.
;;;

(defgeneric apply (fctype actype system)
  (:documentation "Given a non-values ctype of a function, and the non-values ctype of a list of arguments, return a ctype for the return values from CL:APPLYing the function to those arguments."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCALL.
;;;

(defgeneric funcall (system fctype &rest actypes)
  (:documentation "Given a non-values ctype of a function, and a list of argument non-values ctypes, return a ctype for the return values from CL:APPLYing the function to those arguments."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CLASS.
;;;

(defgeneric class (class system)
  (:documentation "Given a class, return the ctype for that class."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CONS.
;;;

(defgeneric cons (car cdr system)
  (:documentation "Given two non-values ctypes, return the ctype of a cons type using them."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions CONSP, CONS-CAR, CONS-CDR.
;;;

(defgeneric consp (ctype system)
  (:documentation "Return true iff the ctype is a cons ctype."))
(defgeneric cons-car (cons-ctype system)
  (:documentation "Given a cons ctype, return its car ctype.

See CONSP"))
(defgeneric cons-cdr (cons-ctype system)
  (:documentation "Given a cons ctype, return its cdr ctype.

See CONSP"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ARRAY.
;;;

(defgeneric array (element dimensions simplicity system)
  (:documentation "Given an element type specifier, dimensions specifier, and simplicity mark, return a ctype representing the array type.
The element type specifier is either an upgraded non-values ctype, or the symbol *.
The dimensions specifier is as in the CL type specifier ARRAY, but must be normalized to a list of dimensions (i.e. 4 is not valid, and must be replaced by (* * * *)).
The simplicity mark is one of the symbols ARRAY or SIMPLE-ARRAY."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions ARRAYP, ARRAY-ELEMENT-TYPE, ARRAY-DIMENSIONS.
;;;

(defgeneric arrayp (ctype system)
  (:documentation "Return true iff the ctype is an array ctype."))
(defgeneric array-element-type (array-ctype system)
  (:documentation "Return the element type of an array ctype.

See ARRAYP"))
(defgeneric array-dimensions (array-ctype system)
  (:documentation "Return the dimensions specifier of an array ctype.

See ARRAYP."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function STRING.
;;;

(defgeneric string (dimension simplicity system)
  (:documentation "Given a dimension specifier, and simplicity mark, return a ctype representing the string type.
The dimension specifier is either a nonnegative integer or the symbol *.
The simplicity mark is one of the symbols ARRAY or SIMPLE-ARRAY."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CHARACTER.
;;;

(defgeneric character (system)
  (:documentation "Return the ctype representing CL:CHARACTER."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BASE-CHAR.
;;;

(defgeneric base-char (system)
  (:documentation "Return the ctype representing CL:BASE-CHAR."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function STANDARD-CHAR.
;;;

(defgeneric standard-char (system)
  (:documentation "Return the ctype representing CL:STANDARD-CHAR."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMPLEX.
;;;

(defgeneric complex (part system)
  (:documentation "Given a part type specifier, return a ctype for the complex type.
The specifier may be either an upgraded non-values ctype or the symbol *."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions COMPLEXP, COMPLEX-PART-TYPE.
;;;

(defgeneric complexp (ctype system)
  (:documentation "Return true if the ctype is a cl:complex ctype."))
(defgeneric complex-part-type (complex-ctype system)
  (:documentation "Return the part type of a cl:complex ctype.

See COMPLEXP"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function RANGE.
;;;

(defgeneric range (kind low high system)
  (:documentation "Given a kind, and interval designators, return a ctype.
This function is used to construct ctypes for ranges of real numbers.
The kind is one of the symbols INTEGER, RATIONAL, REAL, FLOAT, SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, or LONG-FLOAT."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions RANGEP, RANGE-KIND, RANGE-LOW, RANGE-HIGH.
;;;

(defgeneric rangep (ctype system)
  (:documentation "Return true iff the ctype is a range ctype."))
(defgeneric range-kind (range-ctype system)
  (:documentation "Return the kind (e.g. INTEGER) of a range ctype.

See RANGEP"))
(defgeneric range-low (range-ctype system)
  (:documentation "Given a range ctype, return two values: The lower bound, and a boolean indicating whether the bound is exclusive. Non-bounds (i.e. *) are always non-exclusive.

See RANGEP"))
(defgeneric range-high (range-ctype system)
  (:documentation "Given a range ctype, return two values: The upper bound, and a boolean indicating whether the bound is exclusive. Non-bounds (i.e. *) are always non-exclusive.

See RANGEP"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FIXNUM.
;;;

(defgeneric fixnum (system)
  (:documentation "Return the ctype for CL:FIXNUM."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MEMBER.

(defgeneric member (system &rest elements)
  (:documentation "Return the ctype corresponding to (cl:member ...elements...)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions MEMBER-P, MEMBER-MEMBERS.
;;;

(defgeneric member-p (system non-values-ctype)
  (:documentation "Return true iff the ctype is a MEMBER ctype.
Note that EQL types are treated as shorthand for MEMBER types."))
(defgeneric member-members (system member-ctype)
  (:documentation "Return a list of the members of the given member ctype, in arbitrary order.

See MEMBER-P"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SATISFIES.
;;;

(defgeneric satisfies (fname system)
  (:documentation "Return the ctype for (SATISFIES fname)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function KEYWORD.
;;;

(defgeneric keyword (system)
  (:documentation "Return the ctype representing CL:KEYWORD.
While KEYWORD = (SATISFIES KEYWORD), this function exists in case a client has a more specialized representation, for example to make clear that it is a subtype of SYMBOL."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION.
;;;

(defgeneric function
    (required optional rest keyp keys allow-other-keys-p returns system)
  (:documentation "Return a ctype for a function type specifier.

The first seven parameters represent the parsed lambda list.
REQUIRED and OPTIONAL are lists of non-values ctypes, and REST is anon-values ctype. KEYP and ALLOW-OTHER-KEYS-P indicate the presences of &key and &allow-other-keys respectively. KEYS is a list of (keyword non-values-ctype) elements.
RETURNS is the values ctype of the return values specified.

Note that REST must always be provided. If the function does not actually accept more arguments, this should be indicated by REST being the bottom ctype."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMPILED-FUNCTION.
;;;

(defgeneric compiled-function (system)
  (:documentation "Return the ctype for CL:COMPILED-FUNCTION.
While COMPILED-FUNCTION = (SATISFIES COMPILED-FUNCTION-P), this function exists in case a client has a more specialized representation, for example to make clear that it is a subtype of FUNCTION."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VALUES.
;;;

(defgeneric values (required optional rest system)
  (:documentation "Return a ctype for a values type specifier.

The first four parameters represent the parsed lambda list. REQUIRED and OPTIONAL are lists of ctypes. REST is a ctype.

The semantics of values types in the standard are self contradictory.
This function uses the strict semantics described for the VALUES type specifier. A client expecting fuzziness, as it needs to to implement CL:THE, should apply that before using this function. See the parsing in the cleavir-environment system for an example of this.

Strictness means that, for example, (values integer &rest nil) is disjoint from (values integer integer &rest nil), which is very different from how CL:THE works. Strict types are useful for when the compiler can derive exacting types; e.g. (values-list '(nil)) is of type (values null &rest nil) and cannot be zero values.

Values ctypes are only used in certain contexts. See the documentation for other operators for more information."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions VALUES-REQUIRED, VALUES-OPTIONAL, VALUES-REST.
;;;

(defgeneric values-required (ctype system)
  (:documentation "Return the required component of a values ctype."))
(defgeneric values-optional (ctype system)
  (:documentation "Return the optional component of a values ctype."))
(defgeneric values-rest (ctype system)
  (:documentation "Return the rest component of a values ctype."))

(defgeneric nth-value (n ctype system)
  (:argument-precedence-order ctype system n)
  (:documentation "Return the ctype for the Nth value of the given values ctype."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions FUNCTION-REQUIRED, FUNCTION-OPTIONAL, FUNCTION-REST, FUNCTION-KEYSP, FUNCTION-KEYS, FUNCTION-ALLOW-OTHER-KEYS-P,
;;; and FUNCTION-VALUES.
;;;

(defgeneric function-required (ctype system)
  (:documentation "Return the required parameter ctypes of a function ctype."))
(defgeneric function-optional (ctype system)
  (:documentation "Return the optional parameter ctypes of a function ctype."))
(defgeneric function-rest (ctype system)
  (:documentation "Return the rest parameter ctype of a function ctype."))
(defgeneric function-keysp (ctype system)
  (:documentation "Return true iff the function ctype indicates a function that accepts keyword arguments."))
(defgeneric function-keys (ctype system)
  (:documentation "Return the key parameter ctypes for a function ctype. This will be in the form of a list ((keyword ctype) ...)"))
(defgeneric function-allow-other-keys-p (ctype system)
  (:documentation "Return true iff the function ctype indicates a function that accepts unknown keyword arguments."))
(defgeneric function-values (ctype system)
  (:documentation "Return the return values ctype of a function ctype."))
