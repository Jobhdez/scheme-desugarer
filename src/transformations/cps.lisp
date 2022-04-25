(in-package #:manifold-impl/parser)

(defun cps-convert (ast)
  (match ast
   ((int :i a)
     (list  a))
   ((var :v a)
    (list a))))
