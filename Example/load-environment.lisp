(in-package #:cleavir-example)

;;;; Load an example environment full of stuff from the host.

(defun load-environment (env)
  (do-external-symbols (s "CL")
    (cond ((constantp s)
           (%defconstant s (cl:eval s) env))
          (t (multiple-value-bind (expansion expandedp)
                 (macroexpand-1 s)
               (when expandedp
                 (%defsmacro s expansion env)))))
    (cond ((and (fboundp s)
                (not (macro-function s))
                (not (special-operator-p s)))
           (%defun s env))
          (t (let ((pair (assoc s *macros*)))
               (when pair
                 (%defmacro s (cdr pair) env)))))
    (cond ((find-class s nil)
           (%defclass s (find-class s) env))))
  ;; Force computing a policy
  (proclaim-optimize '((safety 1) (debug 1) (speed 1) (space 1)
                       (compilation-speed 1))
                     env)
  (values))
