(in-package #:cleavir-abstract-interpreter)

;;;; Refinements to the reachability analysis based on type information.
;;;; Since the type domain itself depends on reachability (indirectly through
;;;; general data domains' dependency), this has to go into its own file
;;;; dependent on both.

(defmethod interpret-instruction ((strategy strategy) (domain reachability)
                                  (product product) (instruction bir:ifi))
  (let ((dtype-domain (derived-type product)))
    (if dtype-domain
        (let* ((sys (system dtype-domain))
               (input (bir:input instruction))
               (itype (ctype:primary (info strategy dtype-domain input) sys))
               (sblocks (bir:next instruction))
               (false (ctype:member sys nil))
               (true (ctype:negate false sys))
               (thenb (first sblocks))
               (then (bir:start thenb))
               (elseb (second sblocks))
               (else (bir:start elseb)))
          (unless (ctype:disjointp itype false sys)
            (flow-control strategy domain else t))
          (unless (ctype:disjointp itype true sys)
            (flow-control strategy domain then t)))
        (call-next-method))))
