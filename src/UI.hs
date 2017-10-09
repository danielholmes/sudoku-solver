module UI (getSudokuSize, enterPuzzle) where

import Data.List
import Sudoku

stringInIntList :: String -> [Int] -> Bool
stringInIntList _ [] = False
stringInIntList s (x:xs)
    | show x == s = True
    | otherwise   = stringInIntList s xs

getIntOption :: [Int] -> IO Int
getIntOption options =
    do
        putStrLn ("What size Sudoku Puzzle would you like? [" ++ (intercalate ", " (map show options)) ++ "]")
        raw <- getLine
        if stringInIntList raw options
            then return (read raw)
            else do
                putStrLn "That wasn't an option"
                getIntOption options

getSudokuSize :: IO Int
getSudokuSize = getIntOption [4, 9]

enterPuzzle :: Int -> IO Sudoku
enterPuzzle size = return (sudoku size)
