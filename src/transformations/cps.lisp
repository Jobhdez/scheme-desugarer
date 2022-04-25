(in-package #:manifold-transformations)

(defun cps-convert (ast)
  (match
   ((int :i a)
    (make-application :arg (list const-ast a)))
   ((var :v a)
    (make-application :arg (list const-ast a)))
   ))
