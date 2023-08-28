## JoyGP

An attempt at a simple programming language in the spirit of Forth, Joy, and
Push-forth, to be used as the genome encoding format for genetic programming.

#### Goals

1.  Turn an arbitrary `ByteVector` into an executable `JoyGP` without any syntax errors or type errors.
2.  Evolve the program until it achieves a target.


#### How?

* Programs in JoyGP operate on stacks. 
* There is a stack for each allowed datatype.
* Programs are composed of smaller programs.  The smallest programs are elementary operations.
* If an operation expects a value from a stack, and the stack is empty, we just skip it (a `no_op`)!

##### Acknowledgement

The design of this language is heavily inspired by:
* [Push-forth](http://faculty.hampshire.edu/lspector/push.html) and [PushGP](https://lspector.github.io/)
* [Joy & Concatenative Combinators](http://tunes.org/~iepos/joy.html)
* [Scalush](https://github.com/lvilnis/ScalushGP/blob/master/scalushgp.scala) - a prior scala implementation of a similar language
