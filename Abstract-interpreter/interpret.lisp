(in-package #:cleavir-abstract-interpreter)

;;;; Actual definition of INTERPRET-INSTRUCTION.

(defun input-channels (product output-domain)
  (remove-if-not (lambda (channel) (output-domain-p channel output-domain))
                 (channels product)))

(defun outputs-for-domain (strategy product domain instruction)
  (declare (optimize debug))
  (let ((input-channels (input-channels product domain)))
    (if (null input-channels) ; quick case
        (multiple-value-call #'flow-instruction domain instruction
          (instruction-input-info strategy domain instruction))
        ;; MEET all the output infos with that from the domain itself.
        (flet ((outputs-list (channel)
                 (multiple-value-list
                  (multiple-value-call #'flow-instruction channel instruction
                    (instruction-input-info strategy channel instruction))))
               (dmeet (info1 info2) (meet domain info1 info2)))
          (loop with total-output-infos = (outputs-list domain)
                for channel in input-channels
                for output-infos = (outputs-list channel)
                do (map-into total-output-infos #'dmeet
                             total-output-infos output-infos)
                finally (return (values-list total-output-infos)))))))

(defun interpret-one-domain (strategy product domain instruction)
  (multiple-value-call #'instruction-output-info
    strategy domain instruction
    (outputs-for-domain strategy product domain instruction)))

;;; Perform abstract interpretation of an instruction. This should result in
;;; info changes and marking if there is better information.
;;; Called for effect. Internal.
;;; Hypothetically we could have a version that only updates domains with new
;;; info. FIXME?
(defun interpret-instruction (strategy product instruction)
  (loop for domain in (domains product)
        do (interpret-one-domain strategy product domain instruction)))
