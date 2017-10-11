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
findFirstSolution a ep = Prelude.foldr foldStep Nothing (toList (possibleNext a ep))
    where
        foldStep :: Attempt -> Maybe Attempt -> Maybe Attempt
        foldStep n c = c `orElse` finishAttempt n

possibleNext :: Attempt -> SlotPosition -> Set Attempt
possibleNext a ep = Data.Set.map (enterNumber a ep) (possibleValues a ep)

solutionPuzzle :: Solution -> Maybe Puzzle
solutionPuzzle = fmap attemptPuzzle

isSolved :: Attempt -> Bool
isSolved = all isSolvedGroup . attemptGroups

isSolvedGroup :: AttemptGroup -> Bool
isSolvedGroup is = not (any isNothing is) && allUnique (Data.List.map fromJust is)

possibleValues :: Attempt -> SlotPosition -> Set Int
possibleValues a pos = difference available usedInts
    where
        groups = attemptEntryGroups a pos
        available = availableInts (attemptPuzzle a)
        usedInts = fromList (catMaybes (concat groups))
