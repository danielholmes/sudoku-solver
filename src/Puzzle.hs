module Puzzle (
  Puzzle,
  Slot,
  SlotGroup,
  PuzzleGroup,
  SlotPosition,
  puzzleFromEntries,
  emptyPuzzle,
  puzzleEntries,
  puzzleSize,
  puzzleRow,
  puzzleEmptyPositions,
  positions,
  positionGroups,
  positionInt,
  rowGroupPositions,
  colGroupPositions,
  internalGroupPosition,
  groupSize,
  availableInts
) where

import Data.List
import Data.List.Split
import Data.List.Unique
import Data.Maybe
import Data.Set (Set, fromList)

type Slot = Maybe Int

type SlotGroup = [Slot]

type PuzzleGroup = [SlotPosition]

type SlotPosition = (Int, Int)

data Puzzle = PuzzleImpl Int [Slot]

instance Eq Puzzle where
  (==) (PuzzleImpl _ es1) (PuzzleImpl _ es2) = es1 == es2

instance Show Puzzle where
  show (PuzzleImpl s es) = intercalate "\n" (map unwords (chunksOf s (map show es)))

instance Ord Puzzle where
  (PuzzleImpl _ es1) `compare` (PuzzleImpl _ es2) = es1 `compare` es2

puzzleEmptyPositions :: Puzzle -> [SlotPosition]
puzzleEmptyPositions (PuzzleImpl s es) = step 0 es
    where
        step :: Int -> [Slot] -> [SlotPosition]
        step _ [] = []
        step i (Nothing:ess) = puzzleIndexToPosition s i : step (succ i) ess
        step i (_:ess) = step (succ i) ess

puzzleIndexToPosition :: Int -> Int -> SlotPosition
puzzleIndexToPosition size i = (i `mod` size, i `div` size)

puzzlePositionToIndex :: Int -> SlotPosition -> Int
puzzlePositionToIndex size (x, y) = y * size + x

emptyPuzzle :: Int -> Puzzle
emptyPuzzle size = PuzzleImpl size (replicate (size * size) Nothing)

puzzleEntries :: Puzzle -> [Slot]
puzzleEntries (PuzzleImpl _ es) = es

positions :: Puzzle -> [SlotPosition]
positions (PuzzleImpl s _) = map (puzzleIndexToPosition s) [0..(s * s - 1)]

positionInt :: Puzzle -> SlotPosition -> Maybe Int
positionInt (PuzzleImpl s es) pos = es !! puzzlePositionToIndex s pos

availableInts :: Puzzle -> Set Int
availableInts (PuzzleImpl s _) = fromList [1..s]

puzzleSize :: Puzzle -> Int
puzzleSize (PuzzleImpl s _) = s

groupSize :: Puzzle -> Int
groupSize = fromJust . getExactSquare . puzzleSize

puzzleRow :: Puzzle -> Int -> [Slot]
puzzleRow p i
    | i < size  = take size (drop (i * size) (puzzleEntries p))
    | otherwise = error ("Row " ++ show i ++ " not found")
        where size = puzzleSize p

puzzleFromEntries :: [Slot] -> Maybe Puzzle
puzzleFromEntries es = rawPuzzle >>= (\p -> if isValid p then Just p else Nothing)
    where
        size = getExactSquare (length es)
        rawPuzzle = fmap (\sizeInt -> PuzzleImpl sizeInt es) size

isValid :: Puzzle -> Bool
isValid p = all (isValidGroup p) (positionGroups p)

isValidGroup :: Puzzle -> PuzzleGroup -> Bool
isValidGroup p = allUnique . mapMaybe (positionInt p)

getExactSquare :: Int -> Maybe Int
getExactSquare i
    | s * s == i = Just (fromIntegral s)
    | otherwise  = Nothing
        where
            squarer :: Double
            squarer = fromIntegral i
            s = truncate (sqrt squarer)

rowGroupPositions :: Puzzle -> [PuzzleGroup]
rowGroupPositions p@(PuzzleImpl s _) = chunksOf s (positions p)

colGroupPositions :: Puzzle -> [PuzzleGroup]
colGroupPositions = transpose . rowGroupPositions

internalGroupPositions :: Puzzle -> [PuzzleGroup]
internalGroupPositions p = internalGroupPositionsStep 0 0
    where
        dim = groupSize p

        internalGroupPositionsStep :: Int -> Int -> [PuzzleGroup]
        internalGroupPositionsStep x y
            | y >= dim  = []
            | x >= dim  = internalGroupPositionsStep 0 (succ y)
            | otherwise = internalGroupPosition p (x, y) : internalGroupPositionsStep (succ x) y

internalGroupPosition :: Puzzle -> (Int, Int) -> PuzzleGroup
internalGroupPosition p (colX, rowY) = internalGroupStep 0
    where
        dim = groupSize p
        x = colX * dim
        y = rowY * dim
        internalGroupStep :: Int -> [SlotPosition]
        internalGroupStep gy
            | gy >= dim = []
            | otherwise = map (\tx -> (tx,y + gy)) [x..(x + dim -1)] ++ internalGroupStep (succ gy)

positionGroups :: Puzzle -> [PuzzleGroup]
positionGroups p = rowGroupPositions p ++ colGroupPositions p ++ internalGroupPositions p
