module Lib (solverMain) where

import UI
import Display
import Solution

solverMain :: IO ()
solverMain = do
    size <- getSudokuSize
    puzzle <- enterPuzzle size
    let solution = solve puzzle
        puzzleStr = maybe "No solution" attemptToStr solution
    putStrLn puzzleStr
