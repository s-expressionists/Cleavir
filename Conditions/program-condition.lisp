(in-package #:cleavir-conditions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition type PROGRAM-CONDITION
;;;
;;; This is the abstract class of all conditions Cleavir signals to note
;;; problems and anything else about a program it has been input.
;;; For example, a warning about an unused variable is a program condition,
;;; but an internal inconsistency in Cleavir's code is not.
;;; The idea is that program conditions have certain regular features (namely,
;;; source locations). Furthermore, clients may want to tag non-program
;;; conditions as arising from bugs in client code or Cleavir itself.

(define-condition program-condition (acclimation:condition) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition type PROGRAM-ERROR
;;;
;;; Abstract class for program errors. In the context of Cleavir, a "program
;;; error" is a problem with the code that prevents further compilation without
;;; further intervention. This is the definition of compiler-signaled ERRORs
;;; laid out in CLHS 3.2.5 "Exceptional Situations in the Compiler".
;;; Note that Cleavir signals errors where many clients would wish for a warning
;;; or less; for example, CST-to-AST signals an error for unbound variables, but
;;; most Lisp implementations implicitly treat unbound variables as special
;;; variables and signal only a warning. If a client wishes to do this, it must
;;; affirmatively handle Cleavir's condition and signal a warning instead.

(define-condition program-error (program-condition cl:program-error) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition type PROGRAM-WARNING
;;;
;;; Abstract class for program warnings. Here, program warnings indicate that
;;; code can be compiled without outside intervention, but it will have
;;; undefined consequences, or an undesirable error will be signaled at runtime.
;;; Again, this is as laid out in CLHS 3.2.5. Keep in mind that the compiler
;;; signaling a warning results in compilation failure with COMPILE[-FILE].

(define-condition program-warning (program-condition warning) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition type PROGRAM-STYLE-WARNING
;;;
;;; Abstract class for program style warnings. As usual, a style warning is a
;;; warning about a program that has well-defined consequences, but may be
;;; suboptimal or "ugly" in some way, and possibly indicate other problems.
;;; CLHS 3.2.5 gives the example of leaving an un-IGNOREd variable unused.

(define-condition program-style-warning (program-condition style-warning) ())
