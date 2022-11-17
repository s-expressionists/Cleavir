(in-package #:cleavir-example)

(defun load-environment (&optional (env *environment*))
  "Fill the given example environment full of much of the CL package, taken from the host.
Note that most macros are not taken, since they may not expand into portable code."
  (do-external-symbols (s "CL")
    (cond ((constantp s)
           (%defconstant s (cl:eval s) env))
          (t (multiple-value-bind (expansion expandedp)
                 (macroexpand-1 s)
               (when expandedp
                 (%defsmacro s expansion env)))))
    (cond ((or (and (fboundp s)
                    (not (macro-function s))
                    (not (special-operator-p s)))
               (member s *functions*))
           (%defun s env))
          (t (let ((pair (assoc s *macros*)))
               (when pair
                 (%defmacro s (cdr pair) env)))))
    (loop for f in *functions* do (%defun f env))
    (cond ((find-class s nil)
           (%defclass s (find-class s) env))))
  ;; Force computing a policy
  (proclaim-optimize '((safety 1) (debug 1) (speed 1) (space 1)
                       (compilation-speed 1))
                     env)
  (values))
