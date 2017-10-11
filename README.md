# Sudoku Solver

[![Build Status](https://travis-ci.org/danielholmes/sudoku-solver.svg?branch=master)](https://travis-ci.org/danielholmes/sudoku-solver)


## Development Dependencies

 - [Stack](https://haskellstack.org) (tested with version 1.5.1)


## Setting up development

`stack setup`


## Running

```bash
stack build --pedantic
stack exec sudoku-solver-exe
```

Interactively:

```bash
stack build --pedantic
stack repl
```


## Tests

```bash
stack test --pedantic
```


## TODO:
 - Validate puzzle on input and prevent unsolvable - which groups within fromEntries
 - Display fixed vs entered in different colours
 - Offer a by line entry option
 - Try with larger puzzles (16x16)
 - No solution should be in red
 - 1, 1 rest empty on a 9x9 takes long
   - Optimise by short circuiting unsolvable Puzzles
 - look at all step functions for fold opportunities
 