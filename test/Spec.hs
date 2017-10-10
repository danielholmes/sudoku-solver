import Test.Hspec

import Puzzle
import Solution
import Display
import Data.Maybe

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
                es = [Empty, (Fixed 1), (Fixed 2), Empty
                    , (Fixed 4), Empty, (Fixed 1), Empty
                    , Empty, (Fixed 2), (Fixed 3), Empty
                    , Empty, Empty, Empty, (Fixed 4)]
                s = puzzleToStr (fromJust (puzzleFromEntries es))
            in s `shouldBe` "---------\n\
                            \|  1 2  |\n\
                            \|4   1  |\n\
                            \|  2 3  |\n\
                            \|      4|\n\
                            \---------\n"

    describe "Display.puzzleToStrWithMarker" $ do
        it "should generate correct empty 4x4" $
            let s = puzzleToStrWithMarker (emptyPuzzle 4) (1, 0) 'X'
            in s `shouldBe` "---------\n\
                            \|  X    |\n\
                            \|       |\n\
                            \|       |\n\
                            \|       |\n\
                            \---------\n"

    describe "Puzzle.groups" $ do
        it "should return correct subGroups" $
            let
                es = [Empty, (Fixed 2), (Fixed 3), (Entered 4)
                    , (Fixed 3), (Fixed 4), Empty, (Entered 2)
                    , (Fixed 2), (Fixed 3), (Fixed 4), Empty
                    , (Fixed 4), Empty, (Fixed 2), (Entered 3)]
                p = fromJust (puzzleFromEntries es)
                -- Rows
                expected = [[Empty, (Fixed 2), (Fixed 3), (Entered 4)]
                    , [(Fixed 3), (Fixed 4), Empty, (Entered 2)]
                    , [(Fixed 2), (Fixed 3), (Fixed 4), Empty]
                    , [(Fixed 4), Empty, (Fixed 2), (Entered 3)]

                -- Cols
                    , [Empty, (Fixed 3), (Fixed 2), (Fixed 4)]
                    , [(Fixed 2), (Fixed 4), (Fixed 3), Empty]
                    , [(Fixed 3), Empty, (Fixed 4), (Fixed 2)]
                    , [(Entered 4), (Entered 2), Empty, (Entered 3)]

                -- Internal
                    , [Empty, (Fixed 2), (Fixed 3), (Fixed 4)]
                    , [(Fixed 3), (Entered 4), Empty, (Entered 2)]
                    , [(Fixed 2), (Fixed 3), (Fixed 4), Empty]
                    , [(Fixed 4), Empty, (Fixed 2), (Entered 3)]]
              in groups p `shouldBe` expected

    describe "Solution.isSolvedGroup" $ do
        it "should return false when empty included" $
            isSolvedGroup [(Fixed 1), (Fixed 2), (Fixed 3), Empty] `shouldBe` False

        it "should return false when repeated number" $
            isSolvedGroup [(Entered 1), (Entered 2), (Entered 1)] `shouldBe` False

        it "should return true when all unique" $
            isSolvedGroup [(Entered 1), (Entered 2), (Entered 3)] `shouldBe` True

        it "should return true when all unique with mix of entered and fixed" $
            isSolvedGroup [(Entered 1), (Fixed 2), (Entered 3)] `shouldBe` True

        it "should return false when non-unique with mix of entered and fixed" $
            isSolvedGroup [(Entered 1), (Fixed 2), (Entered 2)] `shouldBe` False

    describe "Solution.isSolved" $ do
        it "should return true for correct solution" $
            let
                es = [(Fixed 3), (Fixed 4), (Fixed 1), (Entered 2)
                    , (Fixed 1), (Fixed 2), (Fixed 3), (Entered 4)
                    , (Fixed 2), (Fixed 1), (Fixed 4), (Entered 3)
                    , (Fixed 4), (Fixed 3), (Fixed 2), (Entered 1)]
                p = fromJust (puzzleFromEntries es)
              in isSolved p `shouldBe` True

        it "should return false if any empty" $
            let
                es = [Empty, (Fixed 2), (Fixed 3), (Entered 4)
                    , (Fixed 2), (Fixed 3), (Fixed 4), Empty
                    , (Fixed 3), (Fixed 4), Empty, (Entered 2)
                    , (Fixed 4), Empty, (Fixed 2), (Entered 3)]
                p = fromJust (puzzleFromEntries es)
              in isSolved p `shouldBe` False

        it "should return false for wrong positions" $
            let
                es = [(Fixed 2), (Fixed 1), (Fixed 3), (Entered 4)
                    , (Fixed 2), (Fixed 3), (Fixed 4), (Entered 1)
                    , (Fixed 3), (Fixed 4), (Fixed 1), (Entered 2)
                    , (Fixed 4), (Fixed 1), (Fixed 2), (Entered 3)]
                p = fromJust (puzzleFromEntries es)
              in isSolved p `shouldBe` False
