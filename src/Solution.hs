module Solution (Solution, solve, isSolved, isSolvedGroup, possibleValues, solutionPuzzle) where

import Puzzle
import Data.Maybe
import Data.Generics.Aliases
import Data.List.Unique
import Data.List (map)
import Data.Set

data Solution = Solved Puzzle
    | NotSolvable

solve :: Puzzle -> Solution
solve p
    | isSolved p = Solved p
    | otherwise  = maybe NotSolvable Solved nextSolution
        where
            nextSolution = nextEmptyPosition p >>= findFirstSolution p

findFirstSolution :: Puzzle -> EntryPosition -> Maybe Puzzle
findFirstSolution p ep = step (toList (possibleNext p ep))
    where
        step :: [Puzzle] -> Maybe Puzzle
        step [] = Nothing
        step (s:ss) = orElse (solutionPuzzle (solve s)) (step ss)

possibleNext :: Puzzle -> EntryPosition -> Set Puzzle
possibleNext p ep = Data.Set.map (enterIntoPuzzle p ep) (possibleValues p ep)

solutionPuzzle :: Solution -> Maybe Puzzle
solutionPuzzle (Solved p) = Just p
solutionPuzzle NotSolvable = Nothing

isSolved :: Puzzle -> Bool
isSolved p = all isSolvedGroup (puzzleGroups p)

isSolvedGroup :: EntryGroup -> Bool
isSolvedGroup es = not (any isEmpty es) && allUnique (Data.List.map entryInt es)

possibleValues :: Puzzle -> EntryPosition -> Set Int
possibleValues p pos = difference availableInts usedInts
    where
        groups = entryGroups p pos
        pSize = puzzleSize p
        availableInts = fromList [1..pSize]
        usedInts = fromList (mapMaybe maybeEntryInt (concat groups))
