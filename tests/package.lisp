(fiasco:define-test-package #:manifold-tests
			    (:documentation "Tests for the manifold system.")
			    (:use #:cl
				  #:manifold-scheme)
			    (:import-from #:manifold-scheme)
			    (:export #:run-manifold-tests))
