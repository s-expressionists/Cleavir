(cl:in-package #:cleavir-cst-to-ast)

(defun describe-variable (client environment var-name-cst)
  (let* ((symbol (cst:raw var-name-cst))
         (description (trucler:describe-variable client environment symbol)))
    (loop while (null description)
          do (restart-case (error 'no-variable-description
                                  :name symbol :cst var-name-cst)
               (continue ()
                 :report "Consider the variable as special."
                 (setf description
                       (make-instance 'trucler:special-variable-description
                                      :name symbol :type (ctype:top client))))
               ;; This is identical to CONTINUE, but more specifically named.
               (consider-special ()
                 :report "Consider the variable as special."
                 (setf description
                       (make-instance 'trucler:special-variable-description
                                      :name symbol :type (ctype:top client))))
               (substitute (new-symbol)
                 :report "Substitute a different name."
                 :interactive (lambda ()
                                (format *query-io* "Enter new name: ")
                                (list (read *query-io*)))
                 (setq description (trucler:describe-variable
                                    client environment new-symbol)))))
    description))

(defun describe-function (client environment function-name-cst)
  (let* ((function-name (cst:raw function-name-cst))
         (description (trucler:describe-function client environment function-name)))
    (loop while (null description)
          do (restart-case (error 'no-function-description
                                  :name function-name :cst function-name-cst)
               (consider-global ()
                 :report "Treat it as the name of a global function."
                 (return-from describe-function
                   (make-instance 'trucler:global-function-description
                                  :name function-name
                                  :type (ctype:function-top client))))
               (substitute (new-function-name)
                 :report "Substitute a different name."
                 :interactive (lambda ()
                                (format *query-io* "Enter new name: ")
                                (list (read *query-io*)))
                 (setq description (trucler:describe-function
                                    client environment new-function-name)))))
    description))

(defun describe-tag (client environment tag-name-cst)
  (let* ((tag-name (cst:raw tag-name-cst))
         (description (trucler:describe-tag client environment tag-name)))
    (loop while (null description)
          do (restart-case (error 'no-tag-description
                                  :name tag-name :cst tag-name-cst)
               (substitute (new-tag-name)
                 :report "Substitute a different name."
                 :interactive (lambda ()
                                (format *query-io* "Enter new name: ")
                                (list (read *query-io*)))
                 (setq description (trucler:describe-tag
                                    client environment new-tag-name)))))
    description))

(defun describe-block (client environment block-name-cst)
  (let* ((block-name (cst:raw block-name-cst))
         (description (trucler:describe-block client environment block-name)))
    (loop while (null description)
          do (restart-case (error 'no-block-description
                                  :name block-name :cst block-name-cst)
               (substitute (new-block-name)
                 :report "Substitute a different name."
                 :interactive (lambda ()
                                (format *query-io* "Enter new name: ")
                                (list (read *query-io*)))
                 (setq description (trucler:describe-block
                                    client environment new-block-name)))))
    description))
