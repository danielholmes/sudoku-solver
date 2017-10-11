module UI (getSudokuSize, enterPuzzle) where

import System.IO
import System.Console.ANSI
import Data.List
import Data.Maybe
import Puzzle
import Solution
import Display

getValidatedInput :: (String -> Maybe String) -> Maybe String -> IO String
getValidatedInput validate prompt =
    do
        putStr (fromMaybe "" prompt)
        hFlush stdout
        raw <- getLine
        let err = validate raw
        if isNothing err
            then return raw
            else do
                setSGR [SetColor Foreground Vivid Red]
                putStrLn (fromJust err)
                setSGR [Reset]
                getValidatedInput validate prompt

getOption :: (String -> Maybe String) -> [String] -> Maybe String -> IO String
getOption extraIsValid options prompt = getValidatedInput validate (Just updatedPrompt)
    where
        before = maybe "" (\p -> p ++ "\n") prompt
        updatedPrompt = before ++ "[" ++ intercalate ", " options ++ "]: "
        validate :: String -> Maybe String
        validate v
            | v `notElem` options = Just "That wasn't an option"
            | otherwise           = extraIsValid v

getIntOption :: [Int] -> Maybe String -> IO Int
getIntOption o prompt =
    do
        c <- getOption (const Nothing) (map show o) prompt
        return (read c)

getSudokuSize :: IO Int
getSudokuSize = getIntOption [4, 9] (Just "What size Sudoku Puzzle would you like?")

getSlot :: (String -> Maybe String) -> Int -> IO Slot
getSlot validate s =
    do
        o <- getOption validate options Nothing
        return (strToSlot o)
    where
        options = "" : map show [1..s]

strToSlot :: String -> Slot
strToSlot o = case o of
    "" -> Nothing
    e -> Just (read e)

enterPuzzle :: Int -> IO Puzzle
enterPuzzle s =
    do
        es <- enterRows 0 []
        let puzzle = puzzleFromEntries es
        return (fromJust puzzle)
    where
        validateSlot :: [Slot] -> String -> Maybe String
        validateSlot slots str = if isValid then Nothing else Just "Invalid puzzle"
            where
                slot = strToSlot str
                newSlots = slots ++ [slot]
                puzzle = maybePartialSlotsToPuzzle s newSlots
                isValid = isJust (puzzle >>= solve)

        enterRow :: Int -> Int -> [Slot] -> IO [Slot]
        enterRow x y ss
            | x >= s    = return ss
            | otherwise =
                  do
                      putStrLn (puzzleToStrWithMarker (partialSlotsToPuzzle s ss) (x, y) '░')
                      slot <- getSlot (validateSlot ss) s
                      enterRow (succ x) y (ss ++ [slot])

        enterRows :: Int -> [Slot] -> IO [Slot]
        enterRows y es
            | y >= s    = return es
            | otherwise =
                  do
                      r <- enterRow 0 y es
                      enterRows (succ y) r

partialSlotsToPuzzle :: Int -> [Slot] -> Puzzle
partialSlotsToPuzzle s es = fromJust (maybePartialSlotsToPuzzle s es)

maybePartialSlotsToPuzzle :: Int -> [Slot] -> Maybe Puzzle
maybePartialSlotsToPuzzle s es = puzzleFromEntries (es ++ replicate numLeft Nothing)
    where numLeft = s * s - length es
