# Up-down signatures

This repository contains a library for computing up-down signatures
probabilities.  Included is also a command line tool called `sigscore` which
interfaces with the library.

Up-down signatures are described in:

-   Fink, Willbrand, Brown. *1-D random landscapes and non-random data series*,
    EPL **79**, 2007

The algorithms in this library are a generalization of those described above in
that they can handle the case where each bin of the categorical variable can
hold more than one value.

## The command line tool

The tool reads lines of space or comma separated numbers from `stdin`.  The
first line represents the categorical variable which is intepreted as a list of
integers.  Subsequent lines are intepreted as lists of doubles.  Each line must
have the same number of entries as the first line (missing entries should be
input as `NA`).  The up-down signature (cumulative) probability is calculated
for each line and printed on `stdout`.

## Installation

[Download](https://github.com/b4winckler/sigscore/downloads) a binary version
and put it somewhere in your path.

Alternatively, clone this repository and build it with `cabal`:

    $ git clone https://github.com/b4winckler/sigscore.git
    $ cd sigscore
    $ cabal install

Note that this requires that you have installed a Haskell compiler and `cabal`,
both of which are included in the
[Haskell platform](http://hackage.haskell.org/platform/).
