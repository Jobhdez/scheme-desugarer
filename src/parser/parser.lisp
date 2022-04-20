(defun parse-exp (scheme-exp)
  (match scheme-exp
	 ((guard x (symbolp x)) (make-var :v x))
	 ((guard x (numberp x)) (make-int :i x))
	 ((guard x (scheme-boolp x)) (make-bool :bool x))
	 ((guard x (scheme-letp x)) (make-sLet :l (parse-exp (let-var x))
					       :e (parse-exp (let-right x))
					       :b (parse-exp (let-body x))))
	 ((guard x (scheme-ifp x)) (make-ifscm :cond (parse-exp (if-cond x))
					    :then (parse-exp (if-then x))
					    :else (parse-exp (if-else x))))
	 ((guard x (scheme-lambdap x) (make-lambdascm :var (parse-exp (lambda-var x))
						   :exp (parse-exp (lambda-exp x))
						   :body (parse-exp (lambda-bodyx)))))
	 ))

(defstruct Var v)

(defstruct Int i)

(defstruct sLet l e b)

(defstruct ifscm cond then else)

(defstruct lambdascm var exp body)
