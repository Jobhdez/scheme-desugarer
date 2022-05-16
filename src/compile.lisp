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
	 (body (ompile-ast ast append-preamble)))
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
