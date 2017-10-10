import Test.Hspec

import Puzzle
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
