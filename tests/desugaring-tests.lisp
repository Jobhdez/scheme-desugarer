(in-package #:manifold-tests)

(deftest test-desugar-var ()
  "Test if desugaring VARs work."
  (is (equalp (desugar (manifold-scheme::parse-exp 'x))
	      (manifold-scheme:make-var :v 'x))))

(deftest test-desugar-int ()
  "Test if INTs get desugared."
  (is (equalp (desugar (manifold-scheme:parse-exp 3))
	      (manifold-scheme:make-int :i 3))))

(deftest test-desugar-F-bool ()
  "Test if BOOLs get desugared."
  (is (equalp (desugar (manifold-scheme:parse-exp 'false))
	      (manifold-scheme:make-bool :value 'false))))

(deftest test-desugar-T-int ()
  "Test if BOOLs get desugared."
  (is (equalp (desugar (manifold-scheme:parse-exp 'true))
	      (manifold-scheme:make-bool :value 'true))))

(deftest test-desugar-if ()
  "Test if Ifs get desugared."
  (is (equalp (desugar (manifold-scheme:parse-exp '(if (= 1 1) 1 1)))
	      (parse-exp '(if (= 1 1) 1 1)))))

(deftest test-desugar-set! ()
  "Test if desugaring for Sets! work."
  (is (equalp (desugar (manifold-scheme:parse-exp '(set! d 3)))
	      (parse-exp '(set! d 3)))))

(deftest test-desugar-lambdas ()
  "Test if desugaring for LAMBDAS work."
  (is (equalp (desugar (manifold-scheme:parse-exp '(lambda (n) (let ((x 2)) (+ x n)))))
	      (manifold-scheme:make-lambdascm
	       :var (list (manifold-scheme:make-var :v 'n))
	       :body (manifold-scheme:make-application
		      :operator (manifold-scheme:make-lambdascm
				 :var (list (manifold-scheme:make-var :v 'x))
				 :body (manifold-scheme:make-primitive
					      :op (manifold-scheme:make-var :v '+)
					      :operands (list (manifold-scheme:make-var :v 'x)
							      (manifold-scheme:make-var :v 'n))))
		      :operands (list (manifold-scheme:make-int :i 2)))))))

(deftest test-desugar-lets ()
  "Test if desugaring for LETS work."
  (is (equalp (desugar (manifold-scheme:parse-exp '(let ((x 2)) (+ x n))))
	      (manifold-scheme:make-application
		      :operator (manifold-scheme:make-lambdascm
				 :var (list (manifold-scheme:make-var :v 'x))
				 :body (manifold-scheme:make-primitive
					      :op (manifold-scheme:make-var :v '+)
					      :operands (list (manifold-scheme:make-var :v 'x)
							      (manifold-scheme:make-var :v 'n))))
		      :operands (list (manifold-scheme:make-int :i 2))))))

