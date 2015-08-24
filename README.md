calculator
========

A RPN calculator written in haskell

Building
=========

```
git clone https://github.com/juiko/calculator
cd calculator
stack build  ### or cabal build
```

Usage
=====

If you pass any command line argument, the program will start in interactive mode, where you can enter values and functions, each line will be evaluated
and you will be able to see the values that currently are on the stack.

example:
```
calculator -
1 1 1
---
1.0
1.0
1.0
+
---
2.0
1.0

```

Else the program will read values from stdin and will show the resulting stack.

example:

```
echo "5 7 + fib 5 fact" | calculator
120.0
144.0
```
