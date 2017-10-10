module Puzzle (Puzzle, Entry (Empty, Fixed), puzzleFromEntries, emptyPuzzle, puzzleSize, puzzleRow, entryInt) where

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

puzzleFromEntries :: [Entry] -> Maybe Puzzle
puzzleFromEntries es = fmap (\sizeInt -> PuzzleImpl sizeInt es) size
    where size = getExactSquare (length es)

getExactSquare :: Int -> Maybe Int
getExactSquare i
    | s * s == i = Just (fromIntegral s)
    | otherwise  = Nothing
        where
            squarer :: Double
            squarer = fromIntegral i
            s :: Int
            s = truncate (sqrt squarer)
