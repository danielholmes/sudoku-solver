import Test.Hspec

import Puzzle
import Attempt
import Solution
import Display
import Data.Maybe
import Data.Set

main :: IO ()
main = hspec $ do
    describe "Puzzle.positions" $
        it "should return correct and in order" $
            let
                expected = [(0,0), (1,0), (2,0), (3,0)
                    , (0,1), (1,1), (2,1), (3,1)
                    , (0,2), (1,2), (2,2), (3,2)
                    , (0,3), (1,3), (2,3), (3,3)]
            in positions (emptyPuzzle 4) `shouldBe` expected

    describe "Puzzle.positionGroups" $
        it "should return correct and in order" $
            let
                -- Rows
                expected = [[(0,0), (1,0), (2,0), (3,0)]
                    , [(0,1), (1,1), (2,1), (3,1)]
                    , [(0,2), (1,2), (2,2), (3,2)]
                    , [(0,3), (1,3), (2,3), (3,3)]
                -- Cols
                    , [(0,0), (0,1), (0,2), (0,3)]
                    , [(1,0), (1,1), (1,2), (1,3)]
                    , [(2,0), (2,1), (2,2), (2,3)]
                    , [(3,0), (3,1), (3,2), (3,3)]
                -- Internal
                    , [(0,0), (1,0), (0,1), (1,1)]
                    , [(2,0), (3,0), (2,1), (3,1)]
                    , [(0,2), (1,2), (0,3), (1,3)]
                    , [(2,2), (3,2), (2,3), (3,3)]]
            in positionGroups (emptyPuzzle 4) `shouldBe` expected

    describe "Attempt.attemptGroups" $ do
        it "should return correct subGroups" $
            let
                es = [Nothing, Just 2, Just 3, Just 4
                    , Just 3, Just 4, Nothing, Just 2
                    , Just 2, Just 3, Just 4, Nothing
                    , Just 4, Nothing, Just 2, Just 3]
                a = beginAttempt (fromJust (puzzleFromEntries es))
                -- Rows
                expected = [[Nothing, Just 2, Just 3, Just 4]
                    , [Just 3, Just 4, Nothing, Just 2]
                    , [Just 2, Just 3, Just 4, Nothing]
                    , [Just 4, Nothing, Just 2, Just 3]

                -- Cols
                    , [Nothing, Just 3, Just 2, Just 4]
                    , [Just 2, Just 4, Just 3, Nothing]
                    , [Just 3, Nothing, Just 4, Just 2]
                    , [Just 4, Just 2, Nothing, Just 3]

                -- Internal
                    , [Nothing, Just 2, Just 3, Just 4]
                    , [Just 3, Just 4, Nothing, Just 2]
                    , [Just 2, Just 3, Just 4, Nothing]
                    , [Just 4, Nothing, Just 2, Just 3]]
            in attemptGroups a `shouldBe` expected

        it "should return correct subGroups with entries" $
            let
                es = [Nothing, Nothing, Just 3, Just 4
                    , Just 3, Nothing, Nothing, Just 2
                    , Just 2, Just 3, Just 4, Nothing
                    , Just 4, Nothing, Just 2, Just 3]
                a = beginAttempt (fromJust (puzzleFromEntries es))
                a2 = enterNumber a (1,0) 2
                a3 = enterNumber a2 (1,1) 4
                -- Rows
                expected = [[Nothing, Just 2, Just 3, Just 4]
                    , [Just 3, Just 4, Nothing, Just 2]
                    , [Just 2, Just 3, Just 4, Nothing]
                    , [Just 4, Nothing, Just 2, Just 3]

                -- Cols
                    , [Nothing, Just 3, Just 2, Just 4]
                    , [Just 2, Just 4, Just 3, Nothing]
                    , [Just 3, Nothing, Just 4, Just 2]
                    , [Just 4, Just 2, Nothing, Just 3]

                -- Internal
                    , [Nothing, Just 2, Just 3, Just 4]
                    , [Just 3, Just 4, Nothing, Just 2]
                    , [Just 2, Just 3, Just 4, Nothing]
                    , [Just 4, Nothing, Just 2, Just 3]]
            in attemptGroups a3 `shouldBe` expected

    describe "Attempt.enteredInt" $
        it "should return correct for empty" $
            let modified = beginAttempt (emptyPuzzle 4)
            in enteredInt modified (0,0) `shouldBe` Nothing

    describe "Attempt.enterNumber" $ do
        it "should return correct for single into empty" $
            let
                modified = enterNumber (beginAttempt (emptyPuzzle 4)) (0, 0) 3
            in enteredInt modified (0,0) `shouldBe` Just 3

        it "should return correct for middle insertion" $
            let
                es = [Just 3, Nothing, Nothing, Nothing
                    , Nothing, Nothing, Nothing, Just 4
                    , Just 2, Nothing, Just 2, Nothing
                    , Just 1, Nothing, Nothing, Just 3]
                a = beginAttempt (fromJust (puzzleFromEntries es))
                modified = enterNumber a (2, 3) 2
            in enteredInt modified (2,3) `shouldBe` Just 2

    describe "Attempt.attemptEntryGroups" $ do
        it "should return correct 3 for top left 4x4" $
            let
                es = [Just 3, Nothing, Just 1, Just 2
                    , Just 1, Just 2, Just 3, Just 4
                    , Just 2, Just 1, Just 4, Just 3
                    , Just 4, Nothing, Nothing, Just 1]
                a = beginAttempt (fromJust (puzzleFromEntries es))
                ex = [[Just 3, Nothing, Just 1, Just 2]
                    , [Just 3, Just 1, Just 2, Just 4]
                    , [Just 3, Nothing, Just 1, Just 2]]
            in attemptEntryGroups a (0, 0) `shouldBe` ex

        it "should return correct 3 for bottom right 4x4" $
            let
                es = [Just 3, Nothing, Just 1, Just 2
                    , Just 1, Just 2, Just 3, Just 4
                    , Just 2, Just 1, Just 4, Just 3
                    , Just 4, Nothing, Nothing, Just 1]
                a = beginAttempt (fromJust (puzzleFromEntries es))
                ex = [[Just 4, Nothing, Nothing, Just 1]
                    , [Just 2, Just 4, Just 3, Just 1]
                    , [Just 4, Just 3, Nothing, Just 1]]
            in attemptEntryGroups a (3, 3) `shouldBe` ex

        it "should return correct 3 including entries" $
            let
                es = [Just 3, Nothing, Just 1, Just 2
                    , Just 1, Nothing, Just 3, Just 4
                    , Just 2, Just 1, Just 4, Just 3
                    , Just 4, Nothing, Nothing, Just 1]
                a = beginAttempt (fromJust (puzzleFromEntries es))
                a2 = enterNumber a (1,1) 1
                ex = [[Just 3, Nothing, Just 1, Just 2]
                    , [Just 3, Just 1, Just 2, Just 4]
                    , [Just 3, Nothing, Just 1, Just 1]]
            in attemptEntryGroups a2 (0, 0) `shouldBe` ex

    describe "Solution.isSolvedGroup" $ do
        it "should return false when empty included" $
            isSolvedGroup [Just 1, Just 2, Just 3, Nothing] `shouldBe` False

        it "should return false when repeated number" $
            isSolvedGroup [Just 1, Just 2, Just 1] `shouldBe` False

        it "should return true when all unique" $
            isSolvedGroup [Just 1, Just 2, Just 3] `shouldBe` True

    describe "Solution.isSolved" $ do
        it "should return true for correct solution" $
            let
                es = [Just 3, Just 4, Just 1, Just 2
                    , Just 1, Just 2, Just 3, Just 4
                    , Just 2, Just 1, Just 4, Just 3
                    , Just 4, Just 3, Just 2, Just 1]
                a = beginAttempt (fromJust (puzzleFromEntries es))
            in isSolved a `shouldBe` True

        it "should return true for correct solution with entries" $
            let
                es = [Nothing, Nothing, Just 1, Just 2
                    , Just 1, Just 2, Just 3, Just 4
                    , Just 2, Just 1, Just 4, Just 3
                    , Just 4, Just 3, Just 2, Just 1]
                a = beginAttempt (fromJust (puzzleFromEntries es))
                a2 = enterNumber a (0,0) 3
                a3 = enterNumber a2 (1,0) 4
            in isSolved a3 `shouldBe` True

        it "should return false if any empty" $
            let
                es = [Nothing, Just 2, Just 3, Just 4
                    , Just 2, Just 3, Just 4, Nothing
                    , Just 3, Just 4, Nothing, Just 2
                    , Just 4, Nothing, Just 2, Just 3]
                a = beginAttempt (fromJust (puzzleFromEntries es))
            in isSolved a `shouldBe` False

        it "should return false for wrong positions" $
            let
                es = [Nothing, Just 1, Just 3, Just 4
                    , Just 2, Just 3, Just 4, Just 1
                    , Just 3, Just 4, Just 1, Just 2
                    , Just 4, Just 1, Just 2, Just 3]
                a = beginAttempt (fromJust (puzzleFromEntries es))
                a2 = enterNumber a (0,0) 2
            in isSolved a2 `shouldBe` False

    describe "Solution.nextEmptyPosition" $ do
        it "should return first for all empty" $
            nextEmptyPosition (beginAttempt (emptyPuzzle 9)) `shouldBe` Just (0, 0)

        it "should return none for no empty" $
            let
                es = [Just 3, Just 4, Just 1, Just 2
                    , Just 1, Just 2, Just 3, Just 4
                    , Just 2, Just 1, Just 4, Just 3
                    , Just 4, Just 3, Just 2, Just 1]
                a = beginAttempt (fromJust (puzzleFromEntries es))
            in nextEmptyPosition a `shouldBe` Nothing

        it "should return first for mid empty" $
            let
                es = [Just 3, Just 4, Just 1, Just 2
                    , Just 1, Just 2, Just 3, Just 4
                    , Nothing, Just 1, Just 4, Nothing
                    , Just 4, Just 3, Nothing, Just 1]
                a = beginAttempt (fromJust (puzzleFromEntries es))
            in nextEmptyPosition a `shouldBe` Just (0, 2)

        it "should return first for mid empty with entries" $
            let
                es = [Just 3, Just 4, Nothing, Just 2
                    , Just 1, Just 2, Nothing, Just 4
                    , Nothing, Just 1, Just 4, Nothing
                    , Just 4, Just 3, Nothing, Just 1]
                a = beginAttempt (fromJust (puzzleFromEntries es))
                a2 = enterNumber a (2,0) 1
                a3 = enterNumber a2 (3,0) 3
            in nextEmptyPosition a3 `shouldBe` Just (0, 2)

    describe "Solution.possibleValues" $ do
        it "should return all values for empty puzzle" $
            possibleValues (beginAttempt (emptyPuzzle 9)) (0, 0) `shouldBe` fromList [1..9]

        it "should return available values for fixed" $
            let
                es = [Just 3, Just 4, Just 1, Just 2
                    , Just 1, Nothing, Just 3, Just 4
                    , Just 2, Just 1, Just 4, Just 3
                    , Just 4, Just 3, Just 2, Just 1]
                a = beginAttempt (fromJust (puzzleFromEntries es))
            in possibleValues a (1, 1) `shouldBe` fromList [2]

        it "should return available values for fixed entered mix" $
            let
                es = [Just 3, Just 4, Just 1, Just 2
                    , Just 1, Nothing, Nothing, Just 4
                    , Just 2, Nothing, Just 4, Just 3
                    , Just 4, Just 3, Nothing, Just 1]
                a = beginAttempt (fromJust (puzzleFromEntries es))
                a2 = enterNumber a (2,1) 3
                a3 = enterNumber a2 (1,2) 1
                a4 = enterNumber a3 (2,3) 2
            in possibleValues a4 (1, 1) `shouldBe` fromList [2]

    describe "Solution.solve" $ do
        it "return correct solution when already solved" $
            let
                es = [Just 3, Just 4, Just 1, Just 2
                    , Just 1, Just 2, Just 3, Just 4
                    , Just 2, Just 1, Just 4, Just 3
                    , Just 4, Just 3, Just 2, Just 1]
                p = fromJust (puzzleFromEntries es)
            in solutionPuzzle (solve p) `shouldBe` Just p

        it "return not solved when in full invalid state" $
            let
                es = [Just 3, Just 4, Just 1, Just 2
                    , Just 1, Just 2, Just 3, Just 4
                    , Just 2, Just 1, Just 4, Just 3
                    , Just 4, Just 3, Just 2, Just 4]
                p = fromJust (puzzleFromEntries es)
            in solutionPuzzle (solve p) `shouldBe` Nothing

        it "return correct solution when only one available" $
            let
                es = [Just 3, Just 4, Just 1, Just 2
                    , Just 1, Just 2, Just 3, Just 4
                    , Just 2, Just 1, Just 4, Just 3
                    , Just 4, Just 3, Just 2, Nothing]
                p = fromJust (puzzleFromEntries es)
                ses = [Just 3, Just 4, Just 1, Just 2
                    , Just 1, Just 2, Just 3, Just 4
                    , Just 2, Just 1, Just 4, Just 3
                    , Just 4, Just 3, Just 2, Nothing]
                solution = fromJust (puzzleFromEntries ses)
                a = beginAttempt solution
                solutionA = enterNumber a (3,3) 1
            in solutionAttempt (solve p) `shouldBe` Just solutionA

    describe "Display.puzzleToStr" $ do
        it "should generate correct empty 4x4" $
            let s = puzzleToStr (emptyPuzzle 4)
            in s `shouldBe` "---------\n\
                            \|       |\n\
                            \|       |\n\
                            \|       |\n\
                            \|       |\n\
                            \---------\n"

        it "should generate correct empty 9x9" $
            let s = puzzleToStr (emptyPuzzle 9)
            in s `shouldBe` "-------------------\n\
                            \|                 |\n\
                            \|                 |\n\
                            \|                 |\n\
                            \|                 |\n\
                            \|                 |\n\
                            \|                 |\n\
                            \|                 |\n\
                            \|                 |\n\
                            \|                 |\n\
                            \-------------------\n"

        it "should generate correct populated 4x4" $
            let
                es = [Nothing, Just 1, Just 2, Nothing
                    , Just 4, Nothing, Just 1, Nothing
                    , Nothing, Just 2, Just 3, Nothing
                    , Nothing, Nothing, Nothing, Just 4]
                s = puzzleToStr (fromJust (puzzleFromEntries es))
            in s `shouldBe` "---------\n\
                            \|  1 2  |\n\
                            \|4   1  |\n\
                            \|  2 3  |\n\
                            \|      4|\n\
                            \---------\n"

    describe "Display.attemptToStr" $
        it "should generate correct populated and entered 4x4" $
            let
                es = [Nothing, Nothing, Just 2, Nothing
                    , Just 4, Nothing, Just 1, Nothing
                    , Nothing, Just 2, Just 3, Nothing
                    , Nothing, Nothing, Nothing, Just 4]
                p = fromJust (puzzleFromEntries es)
                a = beginAttempt p
                a2 = enterNumber a (1,0) 1
                s = attemptToStr a2
            in s `shouldBe` "---------\n\
                            \|  1 2  |\n\
                            \|4   1  |\n\
                            \|  2 3  |\n\
                            \|      4|\n\
                            \---------\n"

    describe "Display.puzzleToStrWithMarker" $
        it "should generate correct empty 4x4" $
            let s = puzzleToStrWithMarker (emptyPuzzle 4) (1, 0) 'X'
            in s `shouldBe` "---------\n\
                            \|  X    |\n\
                            \|       |\n\
                            \|       |\n\
                            \|       |\n\
                            \---------\n"
