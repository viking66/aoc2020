module AOC.Day15Spec (spec) where

import Control.Monad ((<=<))
import Test.Hspec

import AOC (day15, parse, part1, part2)

spec :: Spec
spec = do
    describe "day15 part1" $ do
        it "2020th number" $ do
            go1 input `shouldBe` Just 436

go1 :: String -> Maybe Int
go1 = part1 day15 <=< parse day15

input :: String
input = "0,3,6"
