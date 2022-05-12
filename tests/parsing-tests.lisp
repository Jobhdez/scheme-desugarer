(in-package #:manifold-tests)

(deftest test-parse-int ()
  "Test if parsing an int works."
  (is (equalp (manifold-scheme::parse-exp 3)
	      (manifold-scheme:make-int :i 3))))

(deftest test-parse-var ()
  "Test if parsing a var works."
  (is (equalp     (manifold-scheme::parse-exp 'f)
	      (manifold-scheme:make-var :v 'f))))

(deftest test-parse-t-bool ()
  "Test if parsing a bool works."
  (is (equalp     (manifold-scheme::parse-exp 'true)
	      (manifold-scheme:make-bool :value 'true))))

(deftest test-parse-f-bool ()
  "Test if parsing a bool works."
  (is (equalp     (manifold-scheme::parse-exp 'false)
	      (manifold-scheme:make-bool :value 'false))))

(deftest test-parse-let ()
  "Test if parsing a LET expression works."
  (is (equalp     (manifold-scheme::parse-exp '(let ((n 3)) (+ n 3)))
	      (manifold-scheme::make-letscm :var (list (manifold-scheme:make-var :v 'n))
			                   :expr (list (manifold-scheme:make-int :i 3))
			                   :body (manifold-scheme:make-primitive :op (manifold-scheme:make-var :v '+)
					       	                                 :operands (list (manifold-scheme:make-var :v 'n)
						 		                                 (manifold-scheme:make-int :i 3)))))))

(deftest test-parse-letrec ()
  "Test if parsing a LETREC expression works."
  (is (equalp (manifold-scheme::parse-exp '(letrec ((n (lambda (x) (+ x 1)))) (n 4)))
	      (manifold-scheme:make-letrecscm
	       :bindings (list (list (manifold-scheme:make-var :v 'n)
			             (manifold-scheme:make-lambdascm
			         	:var (list (manifold-scheme:make-var :v 'x))
			                :body (manifold-scheme:make-primitive
				               :op (manifold-scheme:make-var :v '+)
				               :operands (list (manifold-scheme:make-var :v 'x) (manifold-scheme:make-int :i 1))))))
	      :expression (manifold-scheme:make-application :operator (manifold-scheme:make-var :v 'n)
							    :operands (list (manifold-scheme:make-int :i 4)))))))

(deftest test-parse-begin ()
  "Test if parsing BEGIN expressions works."
  (is (equalp     (manifold-scheme::parse-exp '(begin (set! d 3) (set! g 4) (+ d g)))
	      (manifold-scheme:make-beginscm :body (list (manifold-scheme:make-setscm :var 'd :rhs (manifold-scheme:make-int :i 3))
					                 (manifold-scheme:make-setscm :var 'g :rhs (manifold-scheme:make-int :i 4)))
			                     :expression (manifold-scheme:make-primitive :op (manifold-scheme:make-var :v '+)
							                                 :operands (list (manifold-scheme:make-var :v 'd)
									                                 (manifold-scheme:make-var :v 'g)))))))

(deftest test-parse-if ()
  "Test if parsing IF expressions works."
  (is (equalp     (manifold-scheme::parse-exp '(if (= 1 1) 1 2))
	      (manifold-scheme:make-ifscm :COND (manifold-scheme:make-primitive :OP (manifold-scheme:make-var :V '=) :OPERANDS (list (manifold-scheme:make-int :I 1) (manifold-scheme:make-int :I 1)))
			                  :THEN (manifold-scheme:make-int :I 1)
			                  :ELSE (manifold-scheme:make-int :I 2)))))

(deftest test-parse-set! ()
  "Test if parsing SET! expression works."
  (is (equalp     (manifold-scheme::parse-exp '(set! d 3))
	      (manifold-scheme:make-setscm :VAR 'D :RHS (manifold-scheme:make-int :I 3)))))

(deftest test-parse-lambda ()
  "Test if parsing LAMBDAS exps works."
  (is (equalp     (manifold-scheme::parse-exp '(lambda (n) (* n n)))
	      (manifold-scheme:make-lambdascm :VAR (list (manifold-scheme:make-var :V 'N))
			                      :BODY (manifold-scheme:make-primitive :OP (manifold-scheme:make-var :V '*)
						                                    :OPERANDS (list (manifold-scheme:make-var :V 'N) (manifold-scheme:make-var :V 'N)))))))

(deftest test-parse-primitive ()
  "Test if parsing PRIMITIVES exps works."
  (is (equalp     (manifold-scheme::parse-exp '(+ 2 3))
	      (manifold-scheme:make-primitive :OP (manifold-scheme:make-var :V '+) :OPERANDS (list (manifold-scheme:make-int :I 2) (manifold-scheme:make-int :I 3))))))

(deftest test-parse-application ()
  "Test if parsing APPLICATION exps works."
  (is (equalp (manifold-scheme::parse-exp '(square 3))
	      (manifold-scheme:make-application :OPERATOR (manifold-scheme:make-var :V 'SQUARE) :OPERANDS (list (manifold-scheme:make-int :I 3))))))
