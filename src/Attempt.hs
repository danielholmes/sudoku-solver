module Attempt (
  Attempt,
  AttemptGroup,
  beginAttempt,
  attemptPuzzle,
  enterNumber,
  emptyPositions,
  nextEmptyPosition,
  enteredInt,
  attemptGroups,
  attemptEntryGroups,
  attemptEntered
) where

import Puzzle
import Data.Map
import Data.Generics.Aliases
import Data.List
import Data.Maybe

data Attempt = AttemptImpl Puzzle (Map SlotPosition (Maybe Int))

type AttemptGroup = [Maybe Int]

-- TODO: Add in entries into display
instance Show Attempt where
  show (AttemptImpl p _) = show p

instance Eq Attempt where
  (==) (AttemptImpl p1 es1) (AttemptImpl p2 es2) = p1 == p2 && es1 == es2

instance Ord Attempt where
  (AttemptImpl p1 es1) `compare` (AttemptImpl p2 es2)
      | p1 == p2  = es1 `compare` es2
      | otherwise = p1 `compare` p2

beginAttempt :: Puzzle -> Attempt
beginAttempt p = AttemptImpl p (fromList (Prelude.map (\pos -> (pos, Nothing)) (puzzleEmptyPositions p)))

attemptPuzzle :: Attempt -> Puzzle
attemptPuzzle (AttemptImpl p _) = p

attemptEntered :: Attempt -> [(SlotPosition, Int)]
attemptEntered (AttemptImpl _ es) = Data.List.map (\(p,Just i) -> (p,i)) (Data.List.filter (isJust . snd) (assocs es))

enterNumber :: Attempt -> SlotPosition -> Int -> Attempt
enterNumber (AttemptImpl p e) pos i = AttemptImpl p (Data.Map.insert pos (Just i) e)

-- TODO: Change to set
emptyPositions :: Attempt -> [SlotPosition]
emptyPositions (AttemptImpl _ es) = Prelude.filter (\k -> isNothing (fromJust (Data.Map.lookup k es))) (keys es)

nextEmptyPosition :: Attempt -> Maybe SlotPosition
nextEmptyPosition = find (const True) . emptyPositions

attemptGroups :: Attempt -> [AttemptGroup]
attemptGroups a@(AttemptImpl p _) = puzzleGroupsToAttemptGroups a (positionGroups p)

puzzleGroupsToAttemptGroups :: Attempt -> [PuzzleGroup] -> [AttemptGroup]
puzzleGroupsToAttemptGroups a = Prelude.map (puzzleGroupToAttemptGroup a)

puzzleGroupToAttemptGroup :: Attempt -> PuzzleGroup -> AttemptGroup
puzzleGroupToAttemptGroup a = Prelude.map (intValue a)

attemptEntryGroups :: Attempt -> SlotPosition -> [AttemptGroup]
attemptEntryGroups a@(AttemptImpl p _) (x,y) = [rowGroups a !! y, colGroups a !! x, internalGroup a (x `div` dim, y `div` dim)]
    where dim = groupSize p

rowGroups :: Attempt -> [AttemptGroup]
rowGroups a@(AttemptImpl p _) = puzzleGroupsToAttemptGroups a (rowGroupPositions p)

colGroups :: Attempt -> [AttemptGroup]
colGroups a@(AttemptImpl p _) = puzzleGroupsToAttemptGroups a (colGroupPositions p)

internalGroup :: Attempt -> (Int, Int) -> AttemptGroup
internalGroup a@(AttemptImpl p _) pos = puzzleGroupToAttemptGroup a (internalGroupPosition p pos)

intValue :: Attempt -> SlotPosition -> Maybe Int
intValue a@(AttemptImpl p _) ep = enteredInt a ep `orElse` positionInt ep p

-- Just for specs atm, TODO: Remove
enteredInt :: Attempt -> SlotPosition -> Maybe Int
enteredInt (AttemptImpl _ es) p = value
    where
        maybeValue :: Maybe (Maybe Int)
        maybeValue = Data.Map.lookup p es
        value :: Maybe Int
        value = maybeValue >>= id
