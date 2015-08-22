calculator
========

A RPN calculator written in haskell

Building
=========

You can build this program using stack (https://github.com/commercialhaskell/stack/)
or cabal.

```
git clone https://github.com/juiko/calculator
cd calculator
stack build
```

Usage
=====

```
stack exec calculator
1 1 +
2 *
### ctrl-d (end of file)
[4]
```
