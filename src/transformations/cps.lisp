(in-package #:manifold-scheme)

(defun atomicp (ast)
  (or (var-p ast)
      (int-p ast)
      (bool-p ast)
      (lambdascm-p ast)))

(defun t-k (ast k)
  (match ast
	 ((guard x (atomicp x))
	  (make-application :operator k :operands (M x)))
	 
	 ((ifscm :cond a :then b :else c)
	  (let* ((cont-parameter (gensym "$RV"))
		 (cont (make-lambdascm :var cont-parameter
				       :body (make-application :operator k
							       :operands cont-parameter))))
	    (T-k a
		 #'(lambda (aexp)
		     (make-ifscm :cond aexp
				 :then (t-c b cont)
				 :else (t-c c cont))))))))

(defstruct application
  "Application Node"
  operator
  operands)
	    
