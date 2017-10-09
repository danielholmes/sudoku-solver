module Solution (Solution, solve, solutionPuzzle) where

import Puzzle

data Solution = SolutionImpl Puzzle (Maybe Puzzle)

solutionPuzzle :: Solution -> Puzzle
solutionPuzzle (SolutionImpl p _) = p

solve :: Puzzle -> Solution
solve p = SolutionImpl p Nothing
