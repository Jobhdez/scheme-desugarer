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
1. This compiler is based on the one built by Dr. Might:  [here](https://matt.might.net/articles/compiling-scheme-to-c/)

2. I would like to thank Robert Smith for giving me feedback and giving me intuition for cps conversion and moreover I would to thank Elias for helping me debug. Both are from the [Coalton Language](https://github.com/coalton-lang/coalton) project.



-- Job



