module Lib (solverMain) where

import UI
import Display
import Solution
import Data.Maybe

solverMain :: IO ()
solverMain = do
    size <- getSudokuSize
    puzzle <- enterPuzzle size
    let solution = fromJust (solve puzzle)
        solutionStr = attemptToStr solution
    putStrLn solutionStr
