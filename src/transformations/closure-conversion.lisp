(in-package #:manifold-scheme)

(defun closure-convert (ast)
  (match ast
	 ((int :i a)
	  (make-int :i a))

	 ((var :v a)
	  (make-var :v a))

	 ((guard x (symbolp x))
	  x)

	 ((bool :value a)
	  (make-bool :value a))

	 ((set-then :var a :rhs b :fn c)
	  (make-set-then :var a
		         :rhs (closure-convert b)
			 :fn c))

	 ((lambdascm :var a :body b)
	  (let* (($env (gensym "ENV"))
		 (*body* (closure-convert b))
		 (*fv* (set-difference (free-variables *body*)
				       (if (not (listp a))
					   (list a)
					 a)
				       :test #'equalp))
		 (id (allocate-environment *fv*))
		 (sub (mapcar (lambda (v) (list v (make-envget :id id :v v :env $env)))
			      *fv*)))
	    (make-closure :lam (make-lambdascm :var `(,$env ,@a) :body (*substitute* sub *body*))
			  :env (make-envmake :id id :fvs (mapcar #'list *fv* *fv*)))))

	 ((ifscm :cond a :then b :else c)
	  (make-ifscm :cond (closure-convert a)
		      :then (closure-convert b)
		      :else (closure-convert c)))

	 ((primitive :op a :operands b)
	  (make-primitive :op a
			  :operands (mapcar
				     (lambda (operand)
				       (closure-convert operand))
				     b)))

	 ((application :operator a :operands b)
	  (make-application :operator (if (symbolp a)
					  a
					(closure-convert a))
			    :operands (if (not (listp b))
					  (mapcar
					   (lambda (operand)
					     (closure-convert operand))
					   (list b))
					(mapcar (lambda (operand)
						  (closure-convert operand))
						b))))))


(defun free-variables (exp)
  (match exp
	 ((int :i a)
	  '())

	 ((var :v a)
	  (list (make-var :v a)))

	 ((cps :op a)
	  '())

	 ((guard x (symbolp x))
	  (list x))

	 ((bool :value a)
	  '())

	 ((lambdascm :var a :body b)
	  (set-difference (free-variables b)
			  (if (symbolp (cdr a))
			      (list (car a) (cdr a))
			    a)
			  :test #'equalp))

	 ((ifscm :cond a :then b :else c)
	  (union (free-variables a)
		 (union (free-variables b)
			(free-variables c))))

	 ((set-then :var a :rhs b :fn c)
	  (union (list a)
		 (free-variables (if (listp b) b (list b)))))

	 ((closure :lam a :env b)
	  (union (free-variables a)
		 (free-variables b)))

	 ((envmake :id a :fvs b)
	  (reduce #'union (mapcar (lambda (fv) (free-variables fv)) (if (and (listp b)
									     (or (symbolp (car b))
										 (var-p (car b))))
									b
								      (car b)))))
	 ((envget :id a :v b :env c)
	  (free-variables c))
				   
									    

	 ((primitive :op a :operands b)
	  (union (free-variables a)
		 (reduce #'union (mapcar (lambda (fv) (free-variables fv))
					 (if (listp b)
					     b
					   (list b))))))
			 

	 ((application :operator a :operands b)
	  (union (free-variables a)
		 (reduce #'union (mapcar (lambda (fv) (free-variables fv))
					 (if (not (listp b))
					     (list b)
					   b)))))))
			 
	  


(defun *substitute* (env exp)
  (cond ((null env)
	 exp)
	((symbolp exp) (substitute-var env exp))
	((int-p exp)
	 exp)
	((var-p exp)
	 (substitute-var env exp))
	((lambdascm-p exp)
	 (make-lambdascm :var (lambdascm-var exp)
			 :body (*substitute* (assq-remove-keys env
							     (lambdascm-var exp))
					   (lambdascm-body exp))))
	((set-then-p exp)
	 (make-set-then :var (substitute-var env (set-then-var exp))
			:rhs (*substitute* env (set-then-rhs exp))
			:fn (set-then-fn exp)))

	((ifscm-p exp)
	 (make-ifscm :cond (*substitute* env (ifscm-cond exp))
		     :then (*substitute* env (ifscm-then exp))
		     :else (*substitute* env (ifscm-else exp))))

	((closure-p exp)
	 (make-closure :lam (*substitute* env (closure-lam exp))
		       :env (*substitute* env (closure-env exp))))
	((envmake-p exp)
	 (let ((fields (mapcar (lambda (s) (car s)) (envmake-fvs exp)))
	       (values (mapcar (lambda (s) (car (cdr s))) (envmake-fvs exp))))
	   (make-envmake :id (envmake-id exp)
		         :fvs (mapcar #'list fields
				             (mapcar (substitute-with env)
					             values)))))
	((envget-p exp)
	 (make-envget :id (envget-id exp)
		      :v (envget-v exp)
		      :env (*substitute* env (envget-env exp))))
	((primitive-p exp)
	 (let* ((*subfvs* (mapcar (lambda (s) (car s)) sub))
	        (*primvars* (append (list (cps-op (primitive-op exp)))
				 (primitive-operands exp)))
	        (notenvget (set-difference *primvars* *subfvs* :test #'equalp)))
	   (cons notenvget (mapcar (lambda (s) (make-envget :id 1 :v s :env (gensym "ENV")))
				   *subfvs*))))
	   

	((application-p exp)
	 (make-application :operator (funcall (substitute-with env) (application-operator exp))
			   :operands (mapcar (substitute-with env)
					     (if (not (listp (application-operands exp)))
						 (list (application-operands exp))
					       (application-operands exp)))))))
	

(defvar num-environments 0)

(defvar environments '())

(defun allocate-environment (fields)
  (let ((id num-environments))
    (setq num-environments (+ 1 num-environments))
    (setq environments (cons (cons id fields) environments))
    id))


(defun substitute-var (env var)
  (let ((sub (assoc var env)))
    (if sub
	(car (cdr sub))
      var)))

(defun substitute-with (env)
  (lambda (exp)
    (*substitute* env exp)))

(defun assq-remove-keys (env keys)
  (if (not (listp keys))
      env
    (assq-remove-keys
     (assq-remove-key env (car keys))
     (cdr keys))))

(defun assq-remove-key (env key)
  (cond ((not (listp env))
	 '())
	 ((equalp (car (car env)) key)
	  (assq-remove-key (cdr env) key))
	 (t (cons (car env) (assq-remove-key (cdr env) key)))))


(defstruct envget id v env)

(defstruct closure lam env)

(defstruct envmake id fvs)
