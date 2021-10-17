# Programming in Haskell
This repository contains solutions to selected programming exercises from
[*Programming in Haskell (2nd edition)*](https://www.goodreads.com/book/show/52573728-programming-in-haskell).

Note that this is a personal learning place that was created while
reading the book. I do encourage others to buy and read it!

Exercises can be found in an [IHaskell](https://github.com/gibiansky/IHaskell)
notebook. In order to view and edit it one can start a Docker container
running Jupyter with IHaskell kernel via
```bash
make ihaskell
```

Interactive examples are placed in separate files under the `src/`
directory. Compilation of one particular file can be done with e.g.
```bash
make src/Adder.hs
```
or in bulk with just `make`.

Compilation output (binaries) can then be found under the `bin/`
directory and executed as usual, for instance: `./bin/Adder`.
