module Display (puzzleToStr, puzzleToStrWithMarker, attemptToStr) where

import Puzzle
import Attempt
import Data.List
import Data.Char

-- TODO: Can be an instance of Show for puzzle?
-- Differentiate between entered and fixed ints?
puzzleToStr :: Puzzle -> String
puzzleToStr p = rowCharsToStr (puzzleToRows p)

puzzleToStrWithMarker :: Puzzle -> (Int, Int) -> Char -> String
puzzleToStrWithMarker p (x, y) c = rowCharsToStr modifiedRows
    where
         rows = puzzleToRows p
         (beforeRows, row: afterRows) = splitAt y rows
         (beforeCols, _: afterCols) = splitAt x row
         modifiedRow = beforeCols ++ (c : afterCols)
         modifiedRows = beforeRows ++ (modifiedRow : afterRows)

rowCharsToStr :: [String] -> String
rowCharsToStr rows = hb ++ "\n" ++ intercalate "\n" (map renderRow rows) ++ "\n" ++ hb ++ "\n"
    where
        hb = horizontalBorder (length rows)

        renderRow :: String -> String
        renderRow cs = "|" ++ unwords (map (:[]) cs) ++ "|"

puzzleToRows :: Puzzle -> [String]
puzzleToRows p = step 0
    where
        size = puzzleSize p
        step :: Int -> [String]
        step i
            | i >= size = []
            | otherwise = rowToChars p i : step (succ i)

horizontalBorder :: Int -> String
horizontalBorder s = intercalate "" (replicate (2 * s + 1) "-")

-- Warning: Only works for single digits atm
rowToChars :: Puzzle -> Int -> String
rowToChars p y = map entryToChar (puzzleRow p y)

entryToChar :: Slot -> Char
entryToChar (Just i) = intToChar i
entryToChar Nothing = ' '

intToChar :: Int -> Char
intToChar = intToDigit

attemptToStr :: Attempt -> String
attemptToStr a = rowCharsToStr eRows
    where
        p = attemptPuzzle a
        rows = puzzleToRows p

        foldStep :: (SlotPosition, Int) -> [String] -> [String]
        foldStep ((x,y), i) rs = beforeRows ++ modifiedRow : afterRows
            where
                (beforeRows,row:afterRows) = splitAt y rs
                (beforeCols,_:afterCols) = splitAt x row
                modifiedRow = beforeCols ++ intToChar i : afterCols

        eRows = foldr foldStep rows (attemptEntered a)
