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
