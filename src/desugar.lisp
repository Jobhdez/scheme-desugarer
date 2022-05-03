(in-package #:manifold-scheme)

(defun desugar (ast)
  (match ast
	 ((var :v a)
	  (make-var :v a))

	 ((int :i a)
	  (make-int :i a))

	 ((bool :value a)
	  (make-bool :value a))

	 ((ifscm :cond a :then b :else c)
	  (make-ifscm :cond a :then b :else c))

	 ((setscm :var a :rhs b)
	  (make-setscm :var a :rhs b))

	 ((lambdascm :var a :body b)
	   (make-lambdascm :var a
			   :body (desugar b)))

	 ((guard x (letscm-p x))
	  (desugar (desugar-letscm x)))

	 ;;((guard x (letrecscm-p x))
	 ;; (desugar (desugar-letrecscm x)))

	 ;;((guard x (beginscm-p x))
	  ;;(desugar (desugar-beginscm x)))

	 ((guard x (primitive-p x))
	  (make-primitive :op (primitive-op x)
			  :operands (mapcar #'(lambda (e) (desugar e))
					    (primitive-operands x))))

	 ((guard x (application-p x))
	  (make-application :operator (desugar (application-operator x))
			    :operands (mapcar #'(lambda (e) (desugar e))
					      (application-operands x))))

	 (_ (error "Invalid Expression"))))

(defun desugar-letscm (ast)
  "Converts a Let expression into a Lambda expression."
  (match ast
	 ((letscm :var a :expr b :body c)
	  (make-application
	   :operator (make-lambdascm :var a :body c)
	   :operands b))
	 (_ (error "Not a Let expression"))))


	 
