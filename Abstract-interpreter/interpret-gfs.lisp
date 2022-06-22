(in-package #:cleavir-abstract-interpreter)

#|
Conceptually, each domain and instruction (program point) pair is associated with
an info pertaining to that domain. This is true regardless of the direction of
the domain, and is true even of data domains (data.lisp) where we don't actually
store the info with the instruction.
Sometimes this "info" is actually a list of info objects. For example for data
domains, there will be one info for each datum. For forward control domains
there will be one info for each of an instruction's successors. It may make more
sense to think of individual infos as being associated with edges - either
control flow edges for control domains, or def/use edges for data domains.

When implementing a domain, FLOW-INSTRUCTION is the main function to write.
FLOW-INSTRUCTION receives as arguments the domain, the instruction, and the list
of infos associated with the instruction. Methods should compute the infos for
the post-instruction point and return them as values.

INTERPRET-INSTRUCTION does the actual side effectual stuff within the interpreter,
and ideally should not need to be customized per-domain.

The point of all this abstraction is basically to make each individual domain
implementation as crystal clear and to the point as possible. They shouldn't have
to reimplement basic aspects of function call semantics, etc.
|#

;;; Mark that an instruction needs reinterpretation.
;;; This may or may not result in immediate interpretation, depending on the strategy.
;;; Called for effect. Internal.
;;; TODO: Could have a version that works per-domain, maybe save some time?
(defgeneric mark (strategy instruction))

;;; Perform abstract interpretation on a module.
(defgeneric interpret-module (strategy product module))

;;; Given an instruction, set its info to the initial state for a strategy and domain.
;;; Called for effect. Internal.
(defgeneric initialize-instruction (strategy domain instruction))

;;; Given an entry point (i.e. a function that could be called from anywhere),
;;; initialize some infos for that appropriately. Used during interpreting a module.
;;; Called for effect. Internal.
(defgeneric initialize-entry-point (strategy domain function))

;;; In the below, a domain can be used as a channel, indicating a scalar channel
;;; from the domain to itself.

;;; Given infos for the input (for forward domains) or output (for backward),
;;; compute infos for the other. Main customization point.
(defgeneric flow-instruction (channel instruction &rest infos))

;;; Return the input infos to pass to FLOW-INSTRUCTION. Internal.
(defgeneric instruction-input-info (strategy channel instruction))

;;; Given the output infos computed by FLOW-INSTRUCTION, store that information
;;; and possibly mark other instructions for reinterpretation.
;;; Internal. Called for effect.
(defgeneric instruction-output-info (strategy domain instruction &rest infos))

;;; Access the information in a domain for a given program object.
(defgeneric info (strategy domain object))
(defgeneric (setf info) (new-info strategy domain object)
  (:argument-precedence-order strategy domain object new-info))
