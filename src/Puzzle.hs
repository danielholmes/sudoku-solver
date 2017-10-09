module Puzzle (emptyPuzzle, Puzzle, Entry, puzzleSize, puzzleRow, entryInt) where

data Entry = Fixed Int
    | Entered Int
    | Empty

data Puzzle = PuzzleImpl Int [Entry]

emptyPuzzle :: Int -> Puzzle
emptyPuzzle size = PuzzleImpl size (replicate (size * size) Empty)

entryInt :: Entry -> Maybe Int
entryInt (Fixed i) = Just i
entryInt (Entered i) = Just i
entryInt _ = Nothing

puzzleEntries :: Puzzle -> [Entry]
puzzleEntries (PuzzleImpl _ es) = es

puzzleSize :: Puzzle -> Int
puzzleSize (PuzzleImpl s _) = s

puzzleRow :: Puzzle -> Int -> [Entry]
puzzleRow p i
    | i < size  = take size (drop (i * size) (puzzleEntries p))
    | otherwise = error ("Row " ++ show i ++ " not found")
        where size = puzzleSize p

--isSquare :: Int -> Bool
--isSquare i = s * s == i
--  where s = sqrt i::Floating
