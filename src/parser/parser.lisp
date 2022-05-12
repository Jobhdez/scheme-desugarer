(in-package #:manifold-scheme)

(defun parse-exp (exp)
  "Parse given EXP."
  (match exp
	 
         ((guard x (numberp x))
	  (make-int :i x))
	 
         ((guard x (scm-boolp x))
	  (make-bool :value x))

	 ((guard x (and (not (scm-boolp x))
			(symbolp x)))
	  (make-var :v x))
	 
         ((guard x (scm-letp x))
	  (make-letscm :var (mapcar (lambda (v) (parse-exp v))
				    (let-var x))
		       :expr (mapcar (lambda (e) (parse-exp e)) (let-exp x))
		       :body (parse-exp (let-body x))))

	 ((guard x (scm-letrecp x))
	  (make-letrecscm :bindings (mapcar (lambda (binding)
					      (list (parse-exp (car binding))
						    (parse-exp (car (cdr binding)))))
					    (letrec-bindings x))
			  :expression (parse-exp (letrec-expression x))))

	 ((guard x (scm-beginscm x))
	  (make-beginscm :body (mapcar (lambda (exp) (parse-exp exp))
				       (begin-body (cdr x)))
			 :expression (parse-exp (begin-exp x))))
	 
         ((guard x (scm-ifp x))
	  (make-ifscm :cond (parse-exp (if-cond x))
		      :then (parse-exp (if-then x))
		      :else (parse-exp (if-else x))))

	 ((guard x (scm-setp x))
	  (make-setscm :var (set-var x)
		       :rhs (parse-exp (set-rhs x))))
  
         ((guard x (scm-lambdap x))
	  (make-lambdascm :var (mapcar (lambda (var) (parse-exp var)) (lambda-var x))
			  :body (parse-exp (lambda-body x))))
  
         ((guard x (scm-primitive-p x))
	  (make-primitive :op (parse-exp (prim-op x))
			  :operands (mapcar #'(lambda (e) (parse-exp e)) (prim-operands x))))

	 ((guard x (scm-applicationp x))
	  (make-application :operator (parse-exp (app-operator x))
			    :operands (mapcar #'(lambda (e)
					       (parse-exp e))
					   (app-operands x))))
	 
         (t error "Not valid expression")))

(defstruct var
  "A Variable Node."
  v)

(defstruct int
  "An Int Node."
  i)

(defstruct bool
  "A Boolean Node."
  value)

(defstruct letscm
  "A Let Node."
  var expr body)

(defstruct letrecscm
  "A Letrec Node."
  bindings
  expression)

(defstruct ifscm
  "An If Node."
  cond then else)

(defstruct lambdascm
  "A Lambda Node."
  var body)

(defstruct setscm
  "A Set! Node."
  var rhs)

(defstruct beginscm
  "A Begin Node."
  body
  expression)
  

(defstruct primitive
  "A Primitive Node."
  op
  operands)

(defstruct application
  "An Application Node."
  operator
  operands)

;;; An Exp is one of:
;;;     - bool
;;;     - let
;;;     - set!
;;;     - if
;;;     - lambda
;;;     - primitive

(defun same-name (x y)
  (string= (symbol-name x) (symbol-name y)))

(defun scm-boolp (exp)
  "Check if Exp is a boolean."
  ;; Exp -> bool
  ;; given: (scm-boolp 'true)
  ;; expect: T
  (and (symbolp exp)
       (or (same-name exp 'true)
	   (same-name exp 'false))))

(defun scm-letp (exp)
  "Check if Exp is a Let expression."
  ;; Exp -> bool
  ;; given: (scm-letp '(let ((n 2)) (* n n)))
  ;; expect: T
  (and (listp exp)
       (same-name (car exp) 'let)))

(defun let-var (exp)
  "Get the variable of the Let Exp."
  ;; Exp -> Exp
  ;; given: (let-var '(let ((n 2)) (* n n)))
  ;; expect: n
  (mapcar (lambda (binding) (car binding))
	  (car (cdr exp))))

(defun let-exp (exp)
  "Get the exp of the Let Exp."
  ;; Exp -> Exp
  ;; given: (let-exp '(let ((n 2)) (* n n)))
  ;; expect: 2
  (mapcar (lambda (binding) (car (cdr binding))) (car (cdr exp))))

(defun let-body (exp)
  "Get the body of the Let Exp."
  ;; Exp -> Exp
  ;; given: (let-body '(let ((n 2)) (* n n)))
  ;; expect: (* n n)
  (car (cdr (cdr exp))))

(defun scm-letrecp (exp)
  "Check if Exp is a Letrec expression."
  ;; given: (scm-letrecp '(letrec ((n (lambda (n) (* n n)))) (n 3)))
  ;; expect: T
  (and (listp exp)
       (symbolp (car exp))
       (same-name (car exp) 'letrec)))

(defun letrec-bindings (exp)
  "Get the bindings of the Letrec expression."
  ;; given: (letrec-bindings '(letrec ((n (lambda (n) (* n n)))) (n 3)))
  ;; expect: ((n (lambda (n) (* n n))))
  (car (cdr exp)))

(defun letrec-expression (exp)
  "Get the Letrec expression."
  ;; given: '(letrec ((n (lambda (n) (* n n)))) (n 3)))
  ;; expect: '(n 3)
  (car (cdr (cdr exp))))

(defun scm-beginscm (exp)
  "Check if Exp is a Begin exp."
  (and (listp exp)
       (symbolp (car exp))
       (same-name (car exp) 'begin)))

(defun begin-body (exp)
  "Gets the body of the begin."
  ;; Exp -> (exp)
  ;; given: (begin-body '(begin (set! d 4) (set! f 5) (+ d f)))
  ;; expect: '((set! d 4) (set! f 5))
  (reverse (cdr (reverse exp))))

(defun begin-exp (exp)
  "Gets the expression - i.e., the last in Exp."
  (car (last exp)))

(defun scm-setp (exp)
  "Check if Exp is a Set! expression."
  ;; Exp -> bool
  ;; given: (scm-setp '(set! d 2))
  ;; expect: T
  (and (listp exp)
       (symbolp (car exp))
       (same-name (car exp) 'set!)))

(defun set-var (exp)
  "Get the variable of the Set! expression."
  ;; Exp -> Exp
  ;; given: (set-var '(set! d 2))
  ;; expect: d
  (car (cdr exp)))

(defun set-rhs (exp)
  "Get the right hand side of the Set! expression."
  ;; Exp -> Exp
  ;; given: (set-rhs '(set! d 2))
  ;; expect: 2
  (car (cdr (cdr exp))))

(defun scm-ifp (exp)
  "Check if the Exp is an If expression."
  ;; Exp -> bool
  ;; given: (scm-ifp '(if (= 2 2) 1 2))
  ;; expect: T
  (and (listp exp)
       (and (symbolp (car exp))
	    (same-name (car exp) 'if))))

(defun if-cond (exp)
  "Get the cond of the If expression."
  ;; Exp -> Exp
  ;; given: (if-cond '(if (= 2 2) 1 2))
  ;; expect: (= 2 2)
  (car (cdr exp)))

(defun if-then (exp)
  "get the consequent of the If expression."
  ;; Exp -> Exp
  ;; given: '(if-then '(if (= 2 2) 1 2))
  ;; expect: 1
  (car (cdr (cdr exp))))

(defun if-else (exp)
  "get the else of the If expression."
  ;; Exp -> Exp
  ;; given: '(if-then '(if (= 2 2) 1 2))
  ;; expect: 2
  (car (cdr (cdr (cdr exp)))))

(defun scm-lambdap (exp)
  "Checks if the Exp is a Lambda expression."
  ;; Exp -> bool
  ;; given: (scm-lambdap '(lambda (n) (* n n)))
  ;; T
  (and (listp exp)
       (symbolp (car exp))
       (same-name (car exp) 'lambda)))

(defun lambda-var (exp)
  "Checks if the Exp is a Lambda expression."
  ;; Exp -> Exp 
  ;; given: (lambda-var '(lambda (n) (* n n)))
  ;; (n)
  (car (cdr exp)))

(defun lambda-body (exp)
  "Checks if the Exp is a Lambda expression."
  ;; Exp -> Exp 
  ;; given: (lambda-body '(lambda (n) (* n n)))
  ;; (* n n)
  (car (cdr (cdr exp))))

(defun scm-primitive-p (exp)
  "Checks if the Exp is a primitive."
  ;; Exp -> bool
  ;; given: (scm-primitive-p '(* 2 2))
  ;; T
  (and (listp exp)
       (member (car exp) '(+ = - / *))))
  

(defun prim-op (exp)
  "Get the operator of the Primitive expression."
  ;; Exp -> Exp
  ;; given: (prim-op '(* 2 2))
  ;; *
  (car exp))

(defun prim-operands (exp)
  "Get the operator of the Primitive expression."
  ;; Exp -> Exp
  ;; given: (prim-operands '(* 2 2))
  ;; (2 2)
  (cdr exp))

(defun scm-applicationp (exp)
  "Check if Exp is an application."
  (listp exp))

(defun app-operator (exp)
  "Get the operator of Exp."
  (car exp))

(defun app-operands (exp)
  "Get the operands of Exp."
  (cdr exp))
