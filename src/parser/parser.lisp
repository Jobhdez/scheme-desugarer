(in-package #:manifold-scheme)

(defun parse-exp (exp)
  "Parse given EXP."
  (match exp
	 ((guard x (and (not (scm-boolp x))
			(symbolp x)))
	  (make-var :v x))
	 
         ((guard x (numberp x))
	  (make-int :i x))
	 
         ((guard x (scm-boolp x))
	  (make-bool :value x))
	 
         ((guard x (scm-letp x))
	  (make-letscm :var (parse-exp (let-var x))
		       :expr (parse-exp (let-exp x))
		       :body (parse-exp (let-body x))))
  
         ((guard x (scm-ifp x))
	  (make-ifscm :cond (parse-exp (if-cond x))
		      :then (parse-exp (if-then x))
		      :else (parse-exp (if-else x))))

	 ((guard x (scm-setp x))
	  (make-setscm :var (set-var x)
		       :rhs (parse-exp (set-rhs x))))
  
         ((guard x (scm-lambdap x))
	  (make-lambdascm :var (lambda-var x)
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

(defstruct ifscm
  "An If Node."
  cond then else)

(defstruct lambdascm
  "A Lambda Node."
  var body)

(defstruct setscm
  "A Set! Node."
  var rhs)

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

(defun scm-boolp (exp)
  "Check if Exp is a boolean."
  ;; Exp -> bool
  ;; given: (scm-boolp 'true)
  ;; expect: T
  (and (not (listp exp))
       (or (equalp exp 'true)
	   (equalp exp 'false))))

(defun scm-letp (exp)
  "Check if Exp is a Let expression."
  ;; Exp -> bool
  ;; given: (scm-letp '(let ((n 2)) (* n n)))
  ;; expect: T
  (and (listp exp)
       (equalp (car exp) 'let)))

(defun let-var (exp)
  "Get the variable of the Let Exp."
  ;; Exp -> Exp
  ;; given: (let-var '(let ((n 2)) (* n n)))
  ;; expect: n
  (car (car (car (cdr exp)))))

(defun let-exp (exp)
  "Get the exp of the Let Exp."
  ;; Exp -> Exp
  ;; given: (let-exp '(let ((n 2)) (* n n)))
  ;; expect: 2
  (car (cdr (car (car (cdr exp))))))

(defun let-body (exp)
  "Get the body of the Let Exp."
  ;; Exp -> Exp
  ;; given: (let-body '(let ((n 2)) (* n n)))
  ;; expect: (* n n)
  (car (cdr (cdr exp))))

(defun scm-setp (exp)
  "Check if Exp is a Set! expression."
  ;; Exp -> bool
  ;; given: (scm-setp '(set! d 2))
  ;; expect: T
  (and (listp exp)
       (equalp (car exp) 'set!)))

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
       (equalp (car exp) 'if)))

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
       (equalp (car exp) 'lambda)))

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
