(in-package #:manifold-scheme)

(defun atomicp (ast)
  (or (var-p ast)
      (int-p ast)
      (bool-p ast)
      (lambdascm-p ast)))

(defun t-k (ast k)
  (match ast
	 ((guard x (atomicp x))
	  (make-application :operator k :operands (m x)))
	 
	 ((ifscm :cond a :then b :else c)
	  (let* ((cont-parameter (gensym "$RV"))
		 (cont (make-lambdascm :var cont-parameter
				       :body (make-application :operator k
							       :operands cont-parameter))))
	    (T-k a
		   (lambda (aexp)
		     (make-ifscm :cond aexp
				 :then (t-c b cont)
				 :else (t-c c cont))))))))

(defun t-c (exp c)
  (match exp
	 ((guard x (atomicp x))
	  (make-application :operator c
			    :operands (M x)))
	 ((ifscm :cond a :then b :else c)
	  (let ((param (gensym "$K")))
	    (eval ``(,,(make-lambdascm :var param
			               :body (t-k a
					            (lambda (aexp)
					               (make-ifscm :cond aexp
							           :then (t-c b param)
							           :else (t-c c param)))))
		     ,,c))))
	 ((primitive :op a :operands b)
	  (t*-k b
		(lambda ($es)
		  `((cps ,a) ,$es ,c))))))

(defun t*-k (exprs k)
  (cond ((null exprs)
	 (k '()))
	((listp exprs)
	 (t-k (car exprs)
	      (lambda (hd)
		(t*-k (cdr exprs)
		      (lambda (tl)
			(k (cons hd tl)))))))))
  
(defun m (aexp)
  (match aexp
	 ((lambdascm :var a :body b)
	  (let ((param (gensym "$K")))
	    (make-lambdascm :var (list a param)
			    :body (t-c b param))))
	 ((int :i a)
	  a)
	 ((var :v a)
	  a)
	 ((bool :value a)
	  a)))
	      


	      

(defstruct application
  "Application Node"
  operator
  operands)
	    
