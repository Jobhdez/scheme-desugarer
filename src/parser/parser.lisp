(in-package #:manifold-scheme)

(defun parse-exp (exp)
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
  
         ((guard x (scm-lambdap x))
	  (make-lambdascm :var (parse-exp (lambda-var x))
			  :expr (parse-exp (lambda-exp x))
			  :body (parse-exp (lambda-bodyx))))
  
         ((guard x (scm-primitive-p x))
	  (make-primitive :op (parse-exp (prim-op x))
			  :operands (mapcar #'(lambda (e) (parse-exp e)) (prim-operands x))))
	 
         (t error "Not valid expression")))

(defstruct var v)

(defstruct int i)

(defstruct bool value)

(defstruct letscm var expr body)

(defstruct ifscm cond then else)

(defstruct lambdascm var expr body)

(defstruct primitive
  op
  operands)

(defun scm-boolp (exp)
  (and (not (listp exp))
       (or (equalp exp 'true)
	   (equalp exp 'false))))

(defun scm-letp (exp)
  (and (listp exp)
       (equalp (car exp) 'let)))

(defun let-var (exp)
  (car (car (car (cdr exp)))))

(defun let-exp (exp)
  (car (cdr (car (car (cdr exp))))))

(defun let-body (exp)
  (car (cdr (cdr exp))))

(defun scm-ifp (exp)
  (and (listp exp)
       (equalp (car exp) 'if)))

(defun if-cond (exp)
  (car (cdr exp)))

(defun if-then (exp)
  (car (cdr (cdr exp))))

(defun if-else (exp)
  (car (cdr (cdr (cdr exp)))))

(defun scm-lambdap (exp)
  (and (listp exp)
       (equalp (car exp) 'lambda)))

(defun scm-primitive-p (exp)
  (listp exp))
  

(defun prim-op (exp)
  (car exp))

(defun prim-operands (exp)
  (cdr exp))
