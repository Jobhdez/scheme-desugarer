(in-package #:manifold-impl/parser)

(defun parse-exp (x)
  (cond ((symbolp x) (make-var :v x))
	 ((numberp x) (make-int :i x))
	 ((scm-boolp x) (make-bool :bool x))
	 ((scm-letp x) (make-sLet :l (parse-exp (let-var x))
					       :e (parse-exp (let-right x))
					       :b (parse-exp (let-body x))))
	 ((scm-ifp x) (make-ifscm :cond (parse-exp (if-cond x))
					    :then (parse-exp (if-then x))
					    :else (parse-exp (if-else x))))
	 ((scm-lambdap x) (make-lambdascm :var (parse-exp (lambda-var x))
						   :exp (parse-exp (lambda-exp x))
						   :body (parse-exp (lambda-bodyx))))
	 ((scm-primitive-p x) (make-primitive :op (parse-exp (prim-op x)) :operands (mapcar #'(lambda (e) (parse-exp e)) (prim-operands x))))
	 (t
	  error "Not valid expression")))

(defstruct var v)

(defstruct int i)

(defstruct slet l e b)

(defstruct ifscm cond then else)

(defstruct lambdascm var exp body)

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

(defun scm-ifp (exp)
  (and (listp exp)
       (equalp (car exp) 'if)))

(defun scm-lambdap (exp)
  (and (listp exp)
       (equalp (car exp) 'lambda)))

(defun scm-primitive-p (exp)
  (listp exp))
  

(defun prim-op (exp)
  (car exp))

(defun prim-operands (exp)
  (cdr exp))

(in-package :manifold-impl/parser)
