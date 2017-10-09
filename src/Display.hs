module Display (puzzleToStr) where

import Puzzle
import Data.List

-- TODO: Can be an instance of Show for puzzle?
-- Differentiate between entered and fixed ints?
puzzleToStr :: Puzzle -> String
puzzleToStr p = hb ++ "\n" ++ intercalate "\n" (step 0) ++ "\n" ++ hb ++ "\n"
    where
        size = puzzleSize p
        hb = horizontalBorder size
        step :: Int -> [String]
        step i
            | i >= size = []
            | otherwise = puzzleRowToStr p i : step (succ i)

horizontalBorder :: Int -> String
horizontalBorder s = intercalate "" (replicate (2 * s + 1) "-")

puzzleRowToStr :: Puzzle -> Int -> String
puzzleRowToStr p row = "|" ++ intercalate " " (map (maybe "░" show) (map entryInt (puzzleRow p row))) ++ "|"
