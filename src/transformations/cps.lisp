(in-package #:manifold-scheme)

(defun cps-convert (ast)
  (match ast
   ((int :i a)
     (list  a))
   ((var :v a)
    (list a))))
