import Test.Hspec

import Puzzle
import Solution
import Display
import Data.Maybe
import Data.Set

main :: IO ()
main = hspec $ do
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
                es = [Empty, Fixed 1, Fixed 2, Empty
                    , Fixed 4, Empty, Fixed 1, Empty
                    , Empty, Fixed 2, Fixed 3, Empty
                    , Empty, Empty, Empty, Fixed 4]
                s = puzzleToStr (fromJust (puzzleFromEntries es))
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

    describe "Puzzle.puzzleGroups" $
        it "should return correct subGroups" $
            let
                es = [Empty, Fixed 2, Fixed 3, Entered 4
                    , Fixed 3, Fixed 4, Empty, Entered 2
                    , Fixed 2, Fixed 3, Fixed 4, Empty
                    , Fixed 4, Empty, Fixed 2, Entered 3]
                p = fromJust (puzzleFromEntries es)
                -- Rows
                expected = [[Empty, Fixed 2, Fixed 3, Entered 4]
                    , [Fixed 3, Fixed 4, Empty, Entered 2]
                    , [Fixed 2, Fixed 3, Fixed 4, Empty]
                    , [Fixed 4, Empty, Fixed 2, Entered 3]

                -- Cols
                    , [Empty, Fixed 3, Fixed 2, Fixed 4]
                    , [Fixed 2, Fixed 4, Fixed 3, Empty]
                    , [Fixed 3, Empty, Fixed 4, Fixed 2]
                    , [Entered 4, Entered 2, Empty, Entered 3]

                -- Internal
                    , [Empty, Fixed 2, Fixed 3, Fixed 4]
                    , [Fixed 3, Entered 4, Empty, Entered 2]
                    , [Fixed 2, Fixed 3, Fixed 4, Empty]
                    , [Fixed 4, Empty, Fixed 2, Entered 3]]
            in puzzleGroups p `shouldBe` expected

    describe "Solution.isSolvedGroup" $ do
        it "should return false when empty included" $
            isSolvedGroup [ Fixed 1, Fixed 2, Fixed 3, Empty] `shouldBe` False

        it "should return false when repeated number" $
            isSolvedGroup [ Entered 1, Entered 2, Entered 1] `shouldBe` False

        it "should return true when all unique" $
            isSolvedGroup [ Entered 1, Entered 2, Entered 3] `shouldBe` True

        it "should return true when all unique with mix of entered and fixed" $
            isSolvedGroup [ Entered 1, Fixed 2, Entered 3] `shouldBe` True

        it "should return false when non-unique with mix of entered and fixed" $
            isSolvedGroup [ Entered 1, Fixed 2, Entered 2] `shouldBe` False

    describe "Solution.isSolved" $ do
        it "should return true for correct solution" $
            let
                es = [Fixed 3, Fixed 4, Fixed 1, Entered 2
                    , Fixed 1, Fixed 2, Fixed 3, Entered 4
                    , Fixed 2, Fixed 1, Fixed 4, Entered 3
                    , Fixed 4, Fixed 3, Fixed 2, Entered 1]
                p = fromJust (puzzleFromEntries es)
            in isSolved p `shouldBe` True

        it "should return false if any empty" $
            let
                es = [Empty, Fixed 2, Fixed 3, Entered 4
                    , Fixed 2, Fixed 3, Fixed 4, Empty
                    , Fixed 3, Fixed 4, Empty, Entered 2
                    , Fixed 4, Empty, Fixed 2, Entered 3]
                p = fromJust (puzzleFromEntries es)
            in isSolved p `shouldBe` False

        it "should return false for wrong positions" $
            let
                es = [Fixed 2, Fixed 1, Fixed 3, Entered 4
                    , Fixed 2, Fixed 3, Fixed 4, Entered 1
                    , Fixed 3, Fixed 4, Fixed 1, Entered 2
                    , Fixed 4, Fixed 1, Fixed 2, Entered 3]
                p = fromJust (puzzleFromEntries es)
            in isSolved p `shouldBe` False

    describe "Puzzle.enterIntoPuzzle" $ do
        it "should return correct for single into empty" $
            let
                ses = [Entered 3, Empty, Empty, Empty
                    , Empty, Empty, Empty, Empty
                    , Empty, Empty, Empty, Empty
                    , Empty, Empty, Empty, Empty]
                expected = fromJust (puzzleFromEntries ses)
            in enterIntoPuzzle (emptyPuzzle 4) (0, 0) 3 `shouldBe` expected

        it "should return correct for middle insertion" $
            let
                es = [Entered 3, Empty, Empty, Empty
                    , Empty, Empty, Empty, Fixed 4
                    , Fixed 2, Empty, Fixed 2, Empty
                    , Fixed 1, Empty, Empty, Fixed 3]
                p = fromJust (puzzleFromEntries es)
                ses = [Entered 3, Empty, Empty, Empty
                    , Empty, Empty, Empty, Fixed 4
                    , Fixed 2, Empty, Fixed 2, Empty
                    , Fixed 1, Empty, Entered 2, Fixed 3]
                expected = fromJust (puzzleFromEntries ses)
            in enterIntoPuzzle p (2, 3) 2 `shouldBe` expected

    describe "Puzzle.nextEmptyPosition" $ do
        it "should return first for all empty" $
            nextEmptyPosition (emptyPuzzle 9) `shouldBe` Just (0, 0)

        it "should return none for no empty" $
            let
                es = [Fixed 3, Fixed 4, Fixed 1, Entered 2
                    , Fixed 1, Fixed 2, Fixed 3, Entered 4
                    , Fixed 2, Fixed 1, Fixed 4, Entered 3
                    , Fixed 4, Fixed 3, Fixed 2, Entered 1]
                p = fromJust (puzzleFromEntries es)
            in nextEmptyPosition p `shouldBe` Nothing

        it "should return first for mid empty" $
            let
                es = [Fixed 3, Fixed 4, Fixed 1, Entered 2
                    , Fixed 1, Fixed 2, Fixed 3, Entered 4
                    , Empty, Fixed 1, Fixed 4, Empty
                    , Fixed 4, Fixed 3, Empty, Entered 1]
                p = fromJust (puzzleFromEntries es)
            in nextEmptyPosition p `shouldBe` Just (0, 2)

    describe "Puzzle.entryGroups" $ do
        it "should return correct 3 for top left 4x4" $
            let
                es = [Fixed 3, Empty, Fixed 1, Entered 2
                    , Fixed 1, Fixed 2, Fixed 3, Entered 4
                    , Fixed 2, Fixed 1, Fixed 4, Entered 3
                    , Fixed 4, Empty, Empty, Entered 1]
                p = fromJust (puzzleFromEntries es)
                ex = [[Fixed 3, Empty, Fixed 1, Entered 2]
                    , [Fixed 3, Fixed 1, Fixed 2, Fixed 4]
                    , [Fixed 3, Empty, Fixed 1, Fixed 2]]
            in entryGroups p (0, 0) `shouldBe` ex

        it "should return correct 3 for bottom right 4x4" $
            let
                es = [Fixed 3, Empty, Fixed 1, Entered 2
                    , Fixed 1, Fixed 2, Fixed 3, Entered 4
                    , Fixed 2, Fixed 1, Fixed 4, Entered 3
                    , Fixed 4, Empty, Empty, Entered 1]
                p = fromJust (puzzleFromEntries es)
                ex = [[Fixed 4, Empty, Empty, Entered 1]
                    , [Entered 2, Entered 4, Entered 3, Entered 1]
                    , [Fixed 4, Entered 3, Empty, Entered 1]]
            in entryGroups p (3, 3) `shouldBe` ex

    describe "Solution.possibleValues" $ do
        it "should return all values for empty puzzle" $
            possibleValues (emptyPuzzle 9) (0, 0) `shouldBe` fromList [1..9]

        it "should return available values for fixed" $
            let
                es = [Fixed 3, Fixed 4, Fixed 1, Fixed 2
                    , Fixed 1, Empty, Fixed 3, Fixed 4
                    , Fixed 2, Fixed 1, Fixed 4, Fixed 3
                    , Fixed 4, Fixed 3, Fixed 2, Fixed 1]
                p = fromJust (puzzleFromEntries es)
            in possibleValues p (1, 1) `shouldBe` fromList [2]

        it "should return available values for fixed entered mix" $
            let
                es = [Fixed 3, Fixed 4, Fixed 1, Fixed 2
                    , Fixed 1, Empty, Entered 3, Fixed 4
                    , Fixed 2, Entered 1, Fixed 4, Fixed 3
                    , Fixed 4, Fixed 3, Entered 2, Fixed 1]
                p = fromJust (puzzleFromEntries es)
            in possibleValues p (1, 1) `shouldBe` fromList [2]

    describe "Solution.solve" $ do
        it "return correct solution when already solved" $
            let
                es = [Fixed 3, Fixed 4, Fixed 1, Fixed 2
                    , Fixed 1, Fixed 2, Fixed 3, Fixed 4
                    , Fixed 2, Fixed 1, Fixed 4, Fixed 3
                    , Fixed 4, Fixed 3, Fixed 2, Entered 1]
                p = fromJust (puzzleFromEntries es)
            in solutionPuzzle (solve p) `shouldBe` Just p

        it "return not solved when in full invalid state" $
            let
                es = [Fixed 3, Fixed 4, Fixed 1, Fixed 2
                    , Fixed 1, Fixed 2, Fixed 3, Fixed 4
                    , Fixed 2, Fixed 1, Fixed 4, Fixed 3
                    , Fixed 4, Fixed 3, Fixed 2, Entered 4]
                p = fromJust (puzzleFromEntries es)
            in solutionPuzzle (solve p) `shouldBe` Nothing

        it "return correct solution when only one available" $
            let
                es = [Fixed 3, Fixed 4, Fixed 1, Fixed 2
                    , Fixed 1, Fixed 2, Fixed 3, Fixed 4
                    , Fixed 2, Fixed 1, Fixed 4, Fixed 3
                    , Fixed 4, Fixed 3, Fixed 2, Empty]
                p = fromJust (puzzleFromEntries es)
                ses = [Fixed 3, Fixed 4, Fixed 1, Fixed 2
                    , Fixed 1, Fixed 2, Fixed 3, Fixed 4
                    , Fixed 2, Fixed 1, Fixed 4, Fixed 3
                    , Fixed 4, Fixed 3, Fixed 2, Entered 1]
                solution = fromJust (puzzleFromEntries ses)
            in solutionPuzzle (solve p) `shouldBe` Just solution
