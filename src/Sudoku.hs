module Sudoku (sudoku, Sudoku, sudokuSize, CompletedSudoku, Entry, completedSudoku) where

type Entry = Maybe Int

data CompletedSudoku = CompletedSudokuImpl Int [Int]

data Sudoku = SudokuImpl Int [Entry]

sudoku :: Int -> Sudoku
sudoku size = SudokuImpl size (replicate (size * size) Nothing)

entryInt :: Entry -> Int
entryInt (Just i) = i
entryInt Nothing = error "No int"

completedSudoku :: Sudoku -> CompletedSudoku
completedSudoku (SudokuImpl s es) = CompletedSudokuImpl s (map entryInt es)

sudokuSize :: Sudoku -> Int
sudokuSize (SudokuImpl s _) = s

--isSquare :: Int -> Bool
--isSquare i = s * s == i
--  where s = sqrt i::Floating
