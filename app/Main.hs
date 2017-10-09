module Main where

import Lib

main :: IO ()
main = do
    size <- getSudokuSize
    puzzle <- enterPuzzle size
    putStrLn ("You chose " ++ show (sudokuSize puzzle))
