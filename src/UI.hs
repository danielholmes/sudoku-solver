module UI (getSudokuSize, enterPuzzle) where

import System.IO
import System.Console.ANSI
import Data.List
import Data.Maybe
import Puzzle
import Display

getOption :: [String] -> Maybe String -> IO String
getOption options prompt =
    do
        let before = maybe "" (\p -> p ++ "\n") prompt
        putStr (before ++ "[" ++ intercalate ", " (map show options) ++ "]: ")
        hFlush stdout
        raw <- getLine
        if raw `elem` options
            then return raw
            else do
                setSGR [SetColor Foreground Vivid Red]
                putStrLn "That wasn't an option"
                setSGR [Reset]
                getOption options prompt

getIntOption :: [Int] -> Maybe String -> IO Int
getIntOption o prompt =
    do
        c <- getOption (map show o) prompt
        return (read c)

getSudokuSize :: IO Int
getSudokuSize = getIntOption [4, 9] (Just "What size Sudoku Puzzle would you like?")

getEntry :: Int -> IO Entry
getEntry s =
    do
        o <- getOption options Nothing
        return (case o of
            "" -> Empty
            e -> Fixed (read e))
    where
        options = "" : map show [1..s]

enterPuzzle :: Int -> IO Puzzle
enterPuzzle s =
    do
        es <- enterRows 0 []
        let puzzle = puzzleFromEntries es
        return (fromJust puzzle)
    where
        enterRow :: Int -> Int -> [Entry] -> IO [Entry]
        enterRow x y es
            | x >= s    = return es
            | otherwise =
                  do
                      putStrLn (puzzleToStrWithMarker (fillPartialPuzzle s es) (x, y) '░')
                      e <- getEntry s
                      enterRow (succ x) y (es ++ [e])

        enterRows :: Int -> [Entry] -> IO [Entry]
        enterRows y es
            | y >= s    = return es
            | otherwise =
                  do
                      r <- enterRow 0 y es
                      enterRows (succ y) r

fillPartialPuzzle :: Int -> [Entry] -> Puzzle
fillPartialPuzzle s es = fromJust (puzzleFromEntries (es ++ replicate numLeft Empty))
    where numLeft = s * s - length es
