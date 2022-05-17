(in-package #:manifold-scheme)

(defun compile (ast)
  (let* ((preamble "")
	 (append-preamble (lambda (s)
			    (setq preamble
				  (concatenate 'string
					       preamble
					       " "
					       s
					       (string #\Newline)))))
	 (body (compile-ast ast append-preamble)))
    (concatenate 'string
		 "int main (int argc, char* argv[]) {"
		 (string #\Newline)
		 preamble
		 " __sum        = MakePrimitive(__prim_sum) ;"
		 (string #\Newline)
		 " __product    = MakePrimitve(__prim_product) ;"
		 (string  #\Newline)
		 " __difference = MakePrimitive(__prim_difference) ;"
		 (string #\Newline)
		 " __display    = MakePrimitive(__prim_display) ;"
		 (string #\Newline)
		 "(__numEqual   = MakePrimitive(__prim_numEqual) ;"
		 (string #\Newline)
		 body
		 (string #\Newline)
		 " return 0;"
		 (string #\Newline)
		 "}"
		 (string #\Newline))))

(defun compile-ast (ast append-preamble)
  (match ast
	 ((guard x (int-p x))
	  (compile-int x))
	 
	 ((guard x (var-p x))
	  (compile-var x))

	 ((guard x (symbolp x))
	  (compile-symbol x))

	 ((guard x (bool-p x))
	  (compile-bool x))

	 ((guard x (ifscm-p x))
	  (compile-ifscm x append-preamble))

	 ((guard x (set-then-p x))
	  (compile-set x))

	 ((guard x (closure-p x))
	  (compile-closure x append-preamble))
	 
	 ((guard x (envmake-p x))
	  (compile-envmake x append-preamble))

	 ((guard x (envget-p x))
	  (compile-envget x append-preamble))

	 ((guard x (primitive-p x))
	  (compile-primitive x append-preamble))

	 ((guard x (application-p x))
	  (compile-application x append-preamble))

	 (_ (error "Unknown expression"))))
	 
	 
					       
(defun compile-int (ast)
  (match ast
	 ((int :i a)
	  (concatenate 'string
		       "MakeInt(" (write-to-string a) ")"))))

(defun compile-bool (ast)
  (match ast
	 ((bool :value a)
	  (concatenate 'string
		       "MakeBoolean("
		       (if (equalp a 'true) "1" "0")
		       ")"))))

(defun compile-var (ast)
  (match ast
	 ((var :v a)
	  (if (member a '(+ - * =))
	      (cond ((string= (symbol-name a) (symbol-name '+))
		     "__sum")
		    ((string= (symbol-name a) (symbol-name '-))
		     "__difference")
		    ((string= (symbol-name a) (symbol-name '*))
		     "__product")
		    (t
		     "__numEqual"))
	    (mangle a)))))

(defun compile-args (args append-preamble)
  (if (not (listp args))
      ""
    (concatenate 'string
		 (compile-exp (car args) append-preamble)
		 (if (listp (cdr args))
		     (concatenate 'string
				  ", "
				  (compile-args (cdr args) append-preamble))
		   ""))))

(defun compile-app (ast append-preamble)
  (let (($tmp (mangle (gensym 'TMP))))
    
    (append-preamble (concatenate 'string
				  "Value "
				  $tmp
				  " ; "))
    
    (let* ((args     (application-operands ast))
           (fun      (application-operator ast)))
      (concatenate 'string
		   "("  $tmp " = " (compile-exp fun append-preamble)
		   ","
		   $tmp ".clo.lam("
		   "MakeEnv(" $tmp ".clo.env)"
		   (if (null args) "" ",")
		   (compile-args args append-preamble) "))"))))

(defun compile-primitive (ast append-preamble)
  (let (($tmp (mangle (gensym 'TMP))))
    
    (append-preamble (concatenate 'string
				  "Value "
				  $tmp
				  " ; "))
    
    (let* ((args     (primitive-operands ast))
           (fun      (primitive-op ast)))
      (concatenate 'string
		   "("  $tmp " = " (compile-exp fun append-preamble)
		   ","
		   $tmp ".clo.lam("
		   "MakeEnv(" $tmp ".clo.env)"
		   (if (null args) "" ",")
		   (compile-args args append-preamble) "))"))))


(defun compile-if (ast append-preamble)
  (concatenate 'string
	       "(" (compile-exp (ifscm-cond ast) append-preamble) ").b.value ? "
	       "(" (compile-exp (ifscm-then ast) append-preamble)      ") : "
	       "(" (compile-exp (ifscm-else ast) append-preamble)      ")"))

(defun compile-env-make (ast append-preamble)
  (concatenate 'string
	       "MakeEnv(__alloc_env"
	       (write-to-string (envmake-id ast))
	       "("
	       (compile-args (envmake-values exp) append-preamble)
	       "))"))

(defun compile-env-get (ast append-preamble)
  (concatenate
   'string 
   "((struct __env_"
   (write-to-string (envget-id ast)) "*)" 
   (compile-exp (envget-env ast) append-preamble) ".env.env)->" 
   (mangle (envget-field ast))))

(defvar num-lambdas 0)


(defvar lambdas '())

(defun allocate-lambda (lam)
  (let ((id num-lambdas))
    (setq num-lambdas (+ 1 num-lambdas))
    (setq lambdas (cons (list id lam) lambdas))
    id))


(defun get-lambda (id)
  (cdr (assq id lambdas)))

(defun compile-closure (ast append-preamble)
  (let* ((lam (closure-lam ast))
         (env (closure-env ast))
         (lid (allocate-lambda (compile-lambda lam))))
    (concatenate
     'string
     "MakeClosure("
     "__lambda_" (write-to-string lid)
     ","
     (compile-exp env append-preamble)
     ")")))

(defun compile-formals (formals)
  (if (not (listp formals))
      ""
    (concatenate
     'string
     "Value "
      (mangle (car formals))
      (if (listp (cdr formals))
          (concatenate 'string  ", " (compile-formals (cdr formals)))
	""))))

(defun compile-lambda (ast)
  (let* ((preamble "")
         (append-preamble (lambda (s)
                            (setq preamble (concatenate 'string  preamble "  " s (string #\Newline))))))
    (let ((formals (compile-formals (lambda-vars ast)))
          (body    (compile-exp     (lambda-body ast) append-preamble)))
      (lambda (name)
        (concatenate 'string  "Value " name "(" formals ") {" (string #\Newline)
                       preamble
                       "  return " body " ;" (string #\Newline)
                       "}"
		       (string #\Newline))))))
  
; c-compile-env-struct : list[symbol] -> string
(define (compile-env-struct ast)
  (let* ((id     (env-id ast))
         (fields (env-fields ast))
         (sid    (write-to-string id))
         (tyname (concatenate 'string  "struct __env_" sid)))
    (concatenate
     'string
     "struct __env_" (write-to-string id) " {" (string #\Newline)
     (apply concatenate 'string (mapcar (lambda (f)
                                 (concatenate 'string
                                  " Value "
                                  (mangle f) 
                                  " ; \n"))
                               fields))
     "} ;" (string #\Newline) (string #\Newline)
     tyname "*" " __alloc_env" sid 
     "(" (compile-formals fields) ")" "{" (string #\Newline)
     "  " tyname "*" " t = malloc(sizeof(" tyname "))" ";" (string #\Newline)
     (apply concatenate 'string 
            (mapcar (lambda (f)
                    (concatenate 'string "  t->" (mangle f) " = " (mangle f) ";" (string #\Newline))
                 fields)))
     "  return t;\n"
     "}" (string #\Newline)
     )))
    
