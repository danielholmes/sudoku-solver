module Puzzle (
  Puzzle,
  Entry (Empty, Fixed, Entered),
  EntryGroup,
  entryInt,
  puzzleFromEntries,
  emptyPuzzle,
  puzzleEntries,
  puzzleSize,
  puzzleRow,
  groups,
  isEmpty
) where

import Data.List.Split
import Data.List
import Data.Maybe

data Entry = Fixed Int
    | Entered Int
    | Empty

type EntryGroup = [Entry]

instance Show Entry where
  show (Fixed i) = show i
  show (Entered i) = show i
  show Empty = " "

instance Eq Entry where
  (==) (Fixed a) (Fixed b) = a == b
  (==) (Entered a) (Entered b) = a == b
  (==) Empty Empty = True
  (==) _ _ = False

instance Ord Entry where
  Empty `compare` _ = LT
  _ `compare` Empty = GT
  (Fixed _) `compare` (Entered _) = GT
  (Entered _) `compare` (Fixed _) = LT
  a `compare` b = entryInt a `compare` entryInt b

data Puzzle = PuzzleImpl Int [Entry]

isEmpty :: Entry -> Bool
isEmpty Empty = True
isEmpty _ = False

entryInt :: Entry -> Int
entryInt (Fixed i) = i
entryInt (Entered i) = i
entryInt Empty = error "No int"

emptyPuzzle :: Int -> Puzzle
emptyPuzzle size = PuzzleImpl size (replicate (size * size) Empty)

puzzleEntries :: Puzzle -> [Entry]
puzzleEntries (PuzzleImpl _ es) = es

puzzleEntrySubList :: Puzzle -> Int -> Int -> Int -> [Entry]
puzzleEntrySubList (PuzzleImpl s es) x y amount
    | x < s && y < s = take amount (drop (y * s + x) es)
    | otherwise      = error "Not within bounds"

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
            s = truncate (sqrt squarer)

groups :: Puzzle -> [EntryGroup]
groups p = rowGroups p ++ colGroups p ++ internalGroups p

rowGroups :: Puzzle -> [EntryGroup]
rowGroups (PuzzleImpl s es) = chunksOf s es

colGroups :: Puzzle -> [EntryGroup]
colGroups = transpose . rowGroups

internalGroups :: Puzzle -> [EntryGroup]
internalGroups p@(PuzzleImpl s es) = internalGroupsStep 0 0
    where
        dim = fromJust (getExactSquare s)

        internalGroupsStep :: Int -> Int -> [EntryGroup]
        internalGroupsStep x y
            | y >= dim  = []
            | x >= dim  = internalGroupsStep 0 (succ y)
            | otherwise = internalGroup p x y : internalGroupsStep (succ x) y

internalGroup :: Puzzle -> Int -> Int -> EntryGroup
internalGroup p@(PuzzleImpl s _) colX rowY = internalGroupStep 0
    where
        dim = fromJust (getExactSquare s)
        x = colX * dim
        y = rowY * dim
        internalGroupStep :: Int -> [Entry]
        internalGroupStep gy
            | gy >= dim = []
            | otherwise = puzzleEntrySubList p x (y + gy) dim ++ internalGroupStep (succ gy)
