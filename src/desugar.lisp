(in-package #:manifold-scheme)

(defun desugar (ast)
  "Desugar the abstract syntax tree."
  ;; given: Node
  ;; expect: Node 
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

	 ((guard x (letrecscm-p x))
	  (desugar (desugar-letrecscm x)))

	 ((guard x (beginscm-p x))
	  (desugar (desugar-beginscm x)))

	 ((guard x (primitive-p x))
	  (make-primitive :op (primitive-op x)
			  :operands (mapcar #'(lambda (e) (desugar e))
					    (primitive-operands x))))

	 ((guard x (application-p x))
	  (make-application :operator (desugar (application-operator x))
			    :operands (mapcar #'(lambda (e) (desugar e))
					      (if (listp (application-operands x))
						  (application-operands x)
						(list (application-operands x))))))

	 (_ (error "Invalid Expression"))))

(defun desugar-letscm (ast)
  "Converts a Let expression into a Lambda expression."
  ;; given: Let Node
  ;; expect: Lambda Node 
  (match ast
	 ((letscm :var a :expr b :body c)
	  (make-application
	   :operator (make-lambdascm :var (if (listp a) a (list a))
				     :body c)
	   :operands b))
	 (_ (error "Not a Let expression"))))

(defun desugar-letrecscm (ast)
  "Converts a Letrec expression into a combination of Let and Begin Nodes."
  (match ast
	 ((letrecscm :bindings a :expression b)
	  (let* ((namings (mapcar
			   (lambda (b)
			     (list (car b) (parse-exp 'false)))
			   a))
		 
		 (sets (mapcar
			(lambda (binding)
			  (make-setscm :var (car binding)
				       :rhs (car (cdr binding))))
			a))
		 
		 (vars (mapcar
			(lambda (binding)
			  (car binding))
			namings))
		 
		 (arguments (mapcar
			     (lambda (binding)
			       (car (cdr binding)))
			     namings)))
	    
	    (make-letscm :var vars
			 :expr arguments
			 :body (make-beginscm :body sets
					      :expression b))))
	 
	 (_ (error "Not a Letrec expression."))))


	 
(defun desugar-beginscm (ast)
  "Converts  a Begin Node into a Let node."
  (match ast
	 ((beginscm :body a :expression b)
	  (let ((exps (append a (list b))))
	    (make-letbind exps)))

	 (_ (error "Not a Begin expression."))))
  

(defun make-letbind (exps)
  "Construct Let structures."
  (if (and (listp exps)
	   (equalp (length exps) 1))
      (car exps)
    (make-letscm :var (gensym "$K")
		 :expr (car exps)
		 :body (make-letbind (cdr exps)))))
    
  
