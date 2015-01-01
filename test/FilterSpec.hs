{-# LANGUAGE OverloadedStrings #-}
module FilterSpec where

import Data.Set (empty, fromList)
import Test.Hspec
import Tools.Filter

spec :: Spec
spec = do
    specRunFilter
    specJoin

specRunFilter :: Spec
specRunFilter = do
    describe "runFilter" $ do
        it "empty keys" $
            runFilter [["a"], ["b"], ["c"]] empty `shouldBe` [["a"], ["b"], ["c"]]
        it "no drop" $
            let keys = fromList ["x", "y", "z"] in
            runFilter [["a"], ["b"], ["c"]] keys `shouldBe` [["a"], ["b"], ["c"]]
        it "drop one" $
            let keys = fromList ["a"] in
            runFilter [["a"], ["b"], ["c"]] keys `shouldBe` [["b"], ["c"]]
        it "drop all" $
            let keys = fromList ["a", "b", "c"] in
            runFilter [["a"], ["b"], ["c"]] keys `shouldBe` []

specJoin :: Spec
specJoin = do
    describe "join" $ do
        it "empty list" $
            join "\t" [] `shouldBe` ""
        it "single element" $
            join "\t" ["hey"] `shouldBe` "hey"
        it "many elements" $
            join "\t" ["apple", "orange", "grape"] `shouldBe` "apple\torange\tgrape"
