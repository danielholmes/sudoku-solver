module Solution (Solution, solve, isSolved, isSolvedGroup, solutionPuzzle) where

import Puzzle
import Data.List.Unique

data Solution = SolutionImpl Puzzle (Maybe Puzzle)

solutionPuzzle :: Solution -> Puzzle
solutionPuzzle (SolutionImpl p _) = p

solve :: Puzzle -> Solution
solve p = SolutionImpl p Nothing

isSolved :: Puzzle -> Bool
isSolved p = all isSolvedGroup (groups p)

isSolvedGroup :: EntryGroup -> Bool
isSolvedGroup es = not (any isEmpty es) && allUnique (map entryInt es)
