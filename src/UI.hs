module UI (getSudokuSize, solvePuzzle) where

import Data.List
import Puzzle
import Solution

stringInIntList :: String -> [Int] -> Bool
stringInIntList _ [] = False
stringInIntList s (x:xs)
    | show x == s = True
    | otherwise   = stringInIntList s xs

getIntOption :: [Int] -> IO Int
getIntOption options =
    do
        putStrLn ("What size Sudoku Puzzle would you like? [" ++ intercalate ", " (map show options) ++ "]")
        raw <- getLine
        if stringInIntList raw options
            then return (read raw)
            else do
                putStrLn "That wasn't an option"
                getIntOption options

getSudokuSize :: IO Int
getSudokuSize = getIntOption [4, 9]

solvePuzzle :: Int -> IO Solution
solvePuzzle size =
    do
        puzzle <- enterPuzzle size
        return (solve puzzle)

enterPuzzle :: Int -> IO Puzzle
enterPuzzle size = return (emptyPuzzle size)
