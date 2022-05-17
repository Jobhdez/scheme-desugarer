# manifold-scheme: a scheme to c compiler

a scheme to c compiler: development in progress

# Using the program 

1. For this you need to clone this repo into a place SBCL can see -- i.e., `~/quicklisp/local-projects/`.
2. Run `(ql:quickload :manifold)`. 
3. If you wan to run the tests then do: `(ql:quickload :manifold/tests)` and then `(asdf:test-system :manifold)`.
5. Run `(in-package #:manifold-scheme)` to use the program.

## System overview

```
+---------+               +------------+
| Parsing |--- [ast] ---> | desugaring |
+---------+               +------------+
                                |
                          [desugared ast]
                                |
                                V
                          +----------------+                   +-------------+
                          | continuation-  |                   |  closure    |
                          | passing style  |---- [cps ast] --->| conversion  |
                          | conversion     |                   |             |
                          +----------------+                   +-------------+
                                                                     |
                                                                  [cc ast]
                                                                     |
                                                                     V
                                                               +------------------+
                                                               | C code generator |
                                                               +------------------+
                                                                        |
                                                                        V
                                                                      [C code]
```


## Acknowledgements
1. This compiler is based on the one built by Dr. Might. I modified some parts and added new ones but the architecture is different. My compiler has a parser, and cps converter. The rest is essentially a fork of Dr. Might's compiler. You can take a look at his compiler [here](https://matt.might.net/articles/compiling-scheme-to-c/)

2. The 90 minute Scheme to C [compiler](https://gist.github.com/nyuichi/1116686) and [slides](http://churchturing.org/y/90-min-scc.pdf) by Dr. Feeley, the creator of Gambit Scheme, helped a lot.



-- Job



