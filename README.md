# project-cis552

In this project, we implement Randomized Algorithms in Haskell.
Names: Claudia Zhu and Zach Sekaran
Pennkeys: jiyunzhu | zsekaran

## Overview of Work Accomplished

In this project, we implemented Randomized Matrix Verification and
SAT approximation in Haskell.

The interesting Haskell part of our implementation is the work done in
RandomGen and StatisticalQC.

## List of Files

#### In /src/

(In viewing order)

- RandomGen.hs
- StatisticalQC.hs
- Matrix.hs
- Sat.hs
- Gen.hs
- State.hs
- Main.hs

#### In /test/

Spec.hs

## Overview of files

The main components are

- RandomGen.hs
- StatisticalQC.hs
- Matrix.hs
- Sat.hs

In _RandomGen_, we describe a random generation monad that
randomly generates samples of a given form. We also give three
instances of this monad using QuickCheck.Gen, IO, and the
random State monad covered in class.

In _StatisticalQC_, we define a function that can check whether
a randomized algorithm works in _expectation_. While QuickCheck
is great for finding counterexamples to properties that should always
be true, we needed methods to quantify the performance of algorithms
with some error tolerance.

The last two modules are where we implemented the algorithms
In _Matrix_, we build out the Matrix and Vector type classes,
and our matrix multiplication verification samples random vectors
from an instance of MonadGen. In _Sat_, we copied much of the type
structure from our homework. The algorithm sampled random valuations,
computed their scores (i.e. # of clauses satisfied),
and output the valuation with the best score.

## Libraries

We use the System.random library version 1.2 to generate stdgens for random values
we use in our code. Under dependencies in package.yaml, add
`random >= 1.2`

## Building, running, and testing

This project compiles with `stack clean; stack build`.
You can run the main executable with `stack run`.
You can run the tests with `stack test`.
