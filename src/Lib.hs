module Lib (solverMain) where

import UI
import Display
import Solution

solverMain :: IO ()
solverMain = do
    size <- getSudokuSize
    solution <- solvePuzzle size
    putStrLn (puzzleToStr (solutionPuzzle solution))

