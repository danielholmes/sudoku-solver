module Puzzle (
  Puzzle,
  Entry (Empty, Fixed, Entered),
  EntryGroup,
  EntryPosition,
  entryInt,
  puzzleFromEntries,
  emptyPuzzle,
  puzzleEntries,
  puzzleSize,
  puzzleRow,
  puzzleGroups,
  isEmpty,
  entryGroups,
  maybeEntryInt,
  nextEmptyPosition,
  enterIntoPuzzle
) where

import Data.List.Split
import Data.List
import Data.Maybe

data Entry = Fixed Int
    | Entered Int
    | Empty

type EntryGroup = [Entry]
type EntryPosition = (Int, Int)

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

instance Eq Puzzle where
  (==) (PuzzleImpl _ es1) (PuzzleImpl _ es2) = es1 == es2

instance Show Puzzle where
  show (PuzzleImpl s es) = intercalate "\n" (map unwords (chunksOf s (map show es)))

instance Ord Puzzle where
  (PuzzleImpl _ es1) `compare` (PuzzleImpl _ es2) = es1 `compare` es2

isEmpty :: Entry -> Bool
isEmpty Empty = True
isEmpty _ = False

entryInt :: Entry -> Int
entryInt = fromJust . maybeEntryInt

maybeEntryInt :: Entry -> Maybe Int
maybeEntryInt (Fixed i) = Just i
maybeEntryInt (Entered i) = Just i
maybeEntryInt Empty = Nothing

nextEmptyPosition :: Puzzle -> Maybe EntryPosition
nextEmptyPosition (PuzzleImpl s es) = step 0 es
    where
        step :: Int -> [Entry] -> Maybe EntryPosition
        step _ [] = Nothing
        step i (Empty:_) = Just (puzzleIndexToPosition s i)
        step i (_:ess) = step (succ i) ess

puzzleIndexToPosition :: Int -> Int -> EntryPosition
puzzleIndexToPosition size i = (i `mod` size, i `div` size)

puzzlePositionToIndex :: Int -> EntryPosition -> Int
puzzlePositionToIndex size (x, y) = y * size + x

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

enterIntoPuzzle :: Puzzle -> EntryPosition -> Int -> Puzzle
enterIntoPuzzle (PuzzleImpl s es) pos i = PuzzleImpl s (before ++ Entered i : after)
    where
        index = puzzlePositionToIndex s pos
        (before,_:after) = splitAt index es

groupSize :: Puzzle -> Int
groupSize = fromJust . getExactSquare . puzzleSize

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

entryGroups :: Puzzle -> EntryPosition -> [EntryGroup]
entryGroups p (x,y) = [rowGroups p !! y, colGroups p !! x, internalGroup p (x `div` dim, y `div` dim)]
    where dim = groupSize p

puzzleGroups :: Puzzle -> [EntryGroup]
puzzleGroups p = rowGroups p ++ colGroups p ++ internalGroups p

rowGroups :: Puzzle -> [EntryGroup]
rowGroups (PuzzleImpl s es) = chunksOf s es

colGroups :: Puzzle -> [EntryGroup]
colGroups = transpose . rowGroups

internalGroups :: Puzzle -> [EntryGroup]
internalGroups p = internalGroupsStep 0 0
    where
        dim = groupSize p

        internalGroupsStep :: Int -> Int -> [EntryGroup]
        internalGroupsStep x y
            | y >= dim  = []
            | x >= dim  = internalGroupsStep 0 (succ y)
            | otherwise = internalGroup p (x, y) : internalGroupsStep (succ x) y

internalGroup :: Puzzle -> (Int, Int) -> EntryGroup
internalGroup p (colX, rowY) = internalGroupStep 0
    where
        dim = groupSize p
        x = colX * dim
        y = rowY * dim
        internalGroupStep :: Int -> [Entry]
        internalGroupStep gy
            | gy >= dim = []
            | otherwise = puzzleEntrySubList p x (y + gy) dim ++ internalGroupStep (succ gy)
