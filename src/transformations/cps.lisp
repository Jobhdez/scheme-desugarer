(in-package #:manifold-scheme)

(defun cps (ast k)
  (match ast
	 ((guard x (atomicp x))
	  (funcall k (cps-atomic x)))
	 
	 ((ifscm :cond a :then b :else c)
	  (let* ((cont-parameter (gensym "$RV"))
		 (cont (make-lambdascm :var cont-parameter
				       :body (make-application :operator k
							       :operands cont-parameter))))
	    (cps   a
		   (lambda (aexp)
		     (make-ifscm :cond aexp
				 :then (cps-convert b cont)
				 :else (cps-convert c cont))))))

	 ((setscm :var a :rhs b)
	  (cps b (lambda (aexp)
		   (make-set-then :var a
				  :rhs aexp
				  :fn (funcall k '(void))))))
	 
	 (_
	  (let* ((param (gensym "$RV"))
		 (cont (make-lambdascm :var param
				       :body (funcall k param))))
	    (cps-convert ast cont)))))
		

(defun cps-convert (exp co)
  (match exp
	 ((guard x (atomicp x))
	  (make-application :operator co :operands (cps-atomic x)))
	 ((ifscm :cond a :then b :else c)
	  (let ((param (gensym "$K")))
	    (make-application :operator
			      (make-lambdascm :var param
			               :body (cps  a
					            #'(lambda (aexp)
							(make-ifscm :cond aexp	
								    :then (cps-convert b param)
							            :else (cps-convert c param)))))
			      :operands co)))

	 ((setscm :var a :rhs b)
	  (cps b (lambda (aexp)
		   (make-set-then :var a
				  :rhs aexp
				  :fn (make-application :operator co
							:operands '(void))))))
	 ((primitive :op a :operands b)
	  (*cps* b
		 (lambda ($es)
		   (make-primitive :op (make-cps :op a)
				   :operands (append $es (list co))))))
		  
	 
	 ((application :operator a :operands b)
	  (let ((app (cps a
			  (lambda ($f)
			    (*cps* b
				   (lambda ($es)
				     `(,$f ,$es ,co)))))))
	    (if (listp app)
		(let* ((op (car app))
	               (operand (car (cdr app)))
	               (halt (car (cdr (cdr app)))))
		  (make-application :operator op
				    :operands (append operand (list halt))))
	      app)))))
	      

(defun *cps* (exprs k)
  (cond ((null exprs)
	 (funcall k '()))
        ((listp exprs)
	   (cps  (car exprs)
	      (lambda (hd)
		(*cps* (cdr exprs)
		       (lambda (tl)
			(funcall k (cons hd tl)))))))))
  
(defun cps-atomic (aexp)
  (match aexp
	 ((lambdascm :var a :body b)
	  (let ((param (gensym "$K")))
	    (make-lambdascm :var (append a (list param))
			    :body (cps-convert b param))))
	 ((int :i a)
	  (make-int :i a))
	 ((var :v a)
	  (make-var :v a))
	 ((bool :value a)
	  (make-bool :value a))))

(defun atomicp (ast)
  (or (var-p ast)
      (int-p ast)
      (bool-p ast)
      (lambdascm-p ast)))
	  

;; Nodes

(defstruct set-then
  "Set-then! Node."
  var rhs fn)

(defstruct cps
  op)
