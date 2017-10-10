module Display (puzzleToStr, puzzleToStrWithMarker) where

import Puzzle
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

rowToChars :: Puzzle -> Int -> String
rowToChars p y = map (maybe ' ' intToDigit . entryInt) (puzzleRow p y)
