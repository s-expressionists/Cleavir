(in-package #:cleavir-abstract-interpreter)

;;;; Refinements to the reachability analysis based on type information.

(defclass type->reachability (scalar-channel)
  ((%input :type type) (%output :type reachability)))

(defmethod flow-instruction ((channel type->reachability) (instruction bir:ifi)
                             &rest infos)
  ;; Only mark the ELSE branch as reachable if NIL is a member of the input's type,
  ;; and similar with THEN.
  (let* ((reach (output channel)) (inf (infimum reach)) (sup (supremum reach))
         (type (input channel)) (client (client type))
         (itype (ctype:primary client (first infos)))
         (false (ctype:member client nil))
         (true (ctype:negate client false)))
    (values (if (ctype:disjointp client itype false) inf sup)
            (if (ctype:disjointp client itype true) inf sup))))
