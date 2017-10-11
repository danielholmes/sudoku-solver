module Solution (
  Solution,
  solve,
  isSolved,
  isSolvedGroup,
  possibleValues,
  solutionPuzzle,
  solutionAttempt
) where

import Puzzle
import Attempt
import Data.Maybe
import Data.Generics.Aliases
import Data.List.Unique
import Data.List (map)
import Data.Set

data Solution = Solved Attempt
    | NotSolvable

solve :: Puzzle -> Solution
solve = finishAttempt . beginAttempt

finishAttempt :: Attempt -> Solution
finishAttempt a
    | isSolved a = Solved a
    | otherwise  = maybe NotSolvable Solved nextSolution
        where
            nextSolution = nextEmptyPosition a >>= findFirstSolution a

findFirstSolution :: Attempt -> SlotPosition -> Maybe Attempt
findFirstSolution a ep = step (toList (possibleNext a ep))
    where
        step :: [Attempt] -> Maybe Attempt
        step [] = Nothing
        step (s:ss) = solutionAttempt (finishAttempt s) `orElse` step ss

possibleNext :: Attempt -> SlotPosition -> Set Attempt
possibleNext a ep = Data.Set.map (enterNumber a ep) (possibleValues a ep)

solutionAttempt :: Solution -> Maybe Attempt
solutionAttempt (Solved a) = Just a
solutionAttempt NotSolvable = Nothing

solutionPuzzle :: Solution -> Maybe Puzzle
solutionPuzzle s = fmap attemptPuzzle (solutionAttempt s)

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
