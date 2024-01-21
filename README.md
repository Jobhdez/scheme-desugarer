# scheme desugar
It desugars Scheme into a tiny core.

# Motivation
Why should you desugar? It makes compilation easier. After the desugaring process you can lower this AST to ANF or another intermediate language. Instead of dealing with all the input language you end up with less complexity!

# Desugarer

Input language:
			 

```
exp ::= number
     | bool
     | var
     | (let ((var exp)) body)
     | (letrec ((var exp)) body)
     | (begin exp exp ...)
     | (set! var exp)
     | (lambda (var) exp)
     | (if exp exp exp)
```

Output language

```
exp ::= number
     | var
     | (lambda (var) exp)
     | (set! VAR exp)
     | (if exp exp exp)
   

