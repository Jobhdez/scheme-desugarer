(asdf:defsystem #:manifold
		:description "A scheme to c compiler :-)"
		:author "Job Hernandez <hj93@protonmail.com>"
		:in-order-to ((asdf:test-op (asdf:test-op #:manifold/tests)))
		:depends-on (#:trivia)
		:serial t
		:pathname "src/"
		:components
		((:file "package")
		 (:module "parser"
			  :components ((:file "parser")))
		 (:file "desugar")
		 (:module "transformations"
			  :components ((:file "cps")
				       (:file "closure-conversion")))))

(asdf:defsystem #:manifold/tests
		:description "Tests for Manifold Scheme."
		:author "Job Hernandez <hj93@protonmail.com>"
		:depends-on (#:manifold
			     #:fiasco)
		:perform (asdf:test-op (o s)
				       (unless (symbol-call :manifold-tests
							    :run-manifold-tests)
					 (error "Tests failed")))
		:pathname "tests/"
		:serial t
		:components ((:file "package")
			     (:file "utilities")
			     (:file "parsing-tests")
			     (:file "desugaring-tests")
			     (:file "cps-conversion-tests")
			     (:file "closure-conversion-tests")))
