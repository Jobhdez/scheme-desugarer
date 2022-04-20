(asdf:defsystem #:manifold
		:description "A scheme to c compiler :-)"
		:author "Job Hernandez <hj93@protonmail.com>"
		:depends-on (#:trivia)
		:serial t
		:pathname "src/"
		:components
		((:file "package")
		 (:module "parser"
			  :serial t
			  :components ((:file "parser")))))
