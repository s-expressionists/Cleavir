(in-package #:cleavir-abstract-interpreter)

;;;; Refinements to the reachability analysis based on type information.

(defclass type->reachability (scalar-channel)
  ((%input :type type) (%output :type reachability)))

(defmethod flow-instruction ((channel type->reachability) (instruction bir:ifi)
                             &rest infos)
  ;; Only mark the ELSE branch as reachable if NIL is a member of the input's type,
  ;; and similar with THEN.
  (let* ((reach (output channel)) (inf (infimum reach)) (sup (supremum reach))
         (type (input channel)) (sys (system type))
         (itype (ctype:primary (first infos) sys))
         (false (ctype:member sys nil))
         (true (ctype:negate false sys)))
    (values (if (ctype:disjointp itype false sys) inf sup)
            (if (ctype:disjointp itype true sys) inf sup))))
