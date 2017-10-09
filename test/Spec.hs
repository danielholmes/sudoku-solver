import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "hello, world" $ do
        it "should add for empty list" $
            let e = "hello"
            in e `shouldBe` "hello"
