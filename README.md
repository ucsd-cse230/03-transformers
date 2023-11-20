# 03-transformers: Transformers and Testing (240 points)

## Overview

The overall objective of this assignment is to get some 
experience using *Monad-Transformers* and *Property-based Testing*. 

The assignment is in the following files that you will modify

- [BST.hs](/src/CSE230/BST.hs)
- [Eval.hs](/src/CSE230/WhilePlus/Eval.hs)

The following file contains useful definitions but does not need to be modified:

- [Types.hs](/src/CSE230/Types.hs)

Finally, there are a`Test.hs` has some sample tests to be used  
to check your assignments before submitting.

- [test/Test.hs](/test/Test.hs)

You should **only modify** the parts of the files which say:

```haskell
error "fill this in"
```

with suitable Haskell implementations. 

**You are free to write and use any helper functions.**

## Instructions

### Assignment Testing and Evaluation

Most of the points, will be awarded automatically, by
**evaluating your functions against a given test suite**.

[Tests.hs](/tests/Test.hs) contains a very small suite
of tests which gives you a flavor of of these tests.
When you run

```shell
$ make test
```

Your last lines should have

```
All N tests passed (...)
OVERALL SCORE = ... / ...
```

**or**

```
K out of N tests failed
OVERALL SCORE = ... / ...
```

**If your output does not have one of the above your code will receive a zero**

If for some problem, you cannot get the code to compile,
leave it as is with the `error "fill me in"` with your 
partial solution enclosed below as a comment.

The other lines will give you a readout for each test.
You are encouraged to try to understand the testing code,
but you will not be graded on this.

Your code *must* typecheck against the given type signatures.
Feel free to add your own tests to this file to exercise the
functions you write. As before, you can compile the code by
doing `stack build` and load in `ghci` by doing `stack ghci`.

**Learn to read the [documentation](http://hackage.haskell.org)**

### Submission Instructions

To submit your code, just do:

```bash
$ make turnin
```
### Collaborators

As before please add the name of any collaborator in the file `COLLABORATORS.md`

## Problem 1: Binary Search Trees

Fill in the missing code in [BST.hs](/src/CSE230/BST.hs)

## Problem 2: An Evaluator for WHILE++

Read **but do not modify** the type and other definitions in [Types.hs](/src/CSE230/WhilePlus/Types.hs)

Fill in the missing code in [Eval.hs](/src/CSE230/WhilePlus/Eval.hs)
