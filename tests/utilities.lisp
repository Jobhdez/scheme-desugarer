(in-package #:manifold-tests)

(defun run-manifold-tests ()
  (run-package-tests
   :packages '(:manifold-tests)
   :interactive t))
