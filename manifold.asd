(asdf:defsystem #:manifold
		:description "A scheme to c compiler :-)"
		:author "Job Hernandez <hj93@protonmail.com>"
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
