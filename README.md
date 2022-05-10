# manifold-scheme: a scheme to c compiler

a scheme to c compiler: development in progress

# Using the program 

1. For this you need to clone this repo into a place SBCL can see -- i.e., `~/quicklisp/local-projects/`.
2. Run `(ql:quickload :manifold)`. 
3. Run `(in-package #:manifold-scheme)`

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
```


## Acknowledgements
1. The articles (about compilers) and an actual compiler in Scheme by Dr. Might were very helpful.

2. The 90 minute Scheme to C [compiler](https://gist.github.com/nyuichi/1116686) and [slides](http://churchturing.org/y/90-min-scc.pdf) by Dr. Feeley, the creator of Gambit Scheme, helped a lot.


-- Job



