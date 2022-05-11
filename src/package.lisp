(defpackage #:manifold-scheme
  (:use #:common-lisp
	#:trivia)
  (:export #:parse-exp
	   #:var
	   #:int
	   #:application
	   #:bool
	   #:primitive
	   #:letscm
	   #:ifscm
	   #:lambdascm
	   #:letrecscm
	   #:setscm
	   #:beginscm
	   #:desugar
	   #:make-int
	   #:make-var
	   #:make-bool
	   #:make-letscm
	   #:make-primitive
	   #:make-letrecscm
	   #:make-lambdascm
	   #:make-application
	   #:make-beginscm
	   #:make-setscm
	   #:make-ifscm
	   #:scm-letrecp
	   #:letrec-bindings
	   #:letrec-expression))
