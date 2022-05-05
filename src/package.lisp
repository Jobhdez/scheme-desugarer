(defpackage #:manifold-scheme
  (:use #:common-lisp
	#:trivia)
  (:export #:parse-exp
	   #:var
	   #:int
	   #:bool
	   #:primitive
	   #:letscm
	   #:ifscm
	   #:lambdascm
	   #:letrecscm
	   #:setscm
	   #:beginscm
	   #:desugar))
