module Solution (
  Solution,
  solve,
  isSolved,
  isSolvedGroup,
  possibleValues,
  solutionPuzzle
) where

import Puzzle
import Attempt
import Data.Maybe
import Data.Generics.Aliases
import Data.List.Unique
import Data.List (map)
import Data.Set

type Solution = Maybe Attempt

solve :: Puzzle -> Solution
solve = finishAttempt . beginAttempt

finishAttempt :: Attempt -> Solution
finishAttempt a
    | isSolved a = Just a
    | otherwise  = nextEmptyPosition a >>= findFirstSolution a

findFirstSolution :: Attempt -> SlotPosition -> Maybe Attempt
findFirstSolution a ep = step (toList (possibleNext a ep))
    where
        step :: [Attempt] -> Maybe Attempt
        step [] = Nothing
        step (s:ss) = finishAttempt s `orElse` step ss

possibleNext :: Attempt -> SlotPosition -> Set Attempt
possibleNext a ep = Data.Set.map (enterNumber a ep) (possibleValues a ep)

solutionPuzzle :: Solution -> Maybe Puzzle
solutionPuzzle = fmap attemptPuzzle

isSolved :: Attempt -> Bool
isSolved = all isSolvedGroup . attemptGroups

isSolvedGroup :: AttemptGroup -> Bool
isSolvedGroup is = not (any isNothing is) && allUnique (Data.List.map fromJust is)

possibleValues :: Attempt -> SlotPosition -> Set Int
possibleValues a pos = difference availableInts usedInts
    where
        groups = attemptEntryGroups a pos
        pSize = puzzleSize (attemptPuzzle a)
        availableInts = fromList [1..pSize]
        usedInts = fromList (catMaybes (concat groups))
