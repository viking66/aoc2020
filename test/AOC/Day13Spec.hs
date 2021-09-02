module AOC.Day13Spec (spec) where

import Control.Monad ((<=<))
import Test.Hspec

import AOC (day13, parse, part1, part2)

spec :: Spec
spec = do
    describe "day13 part1" $ do
        it "Earliest bus" $ do
            go1 input `shouldBe` Just 295
    describe "day13 part2" $ do
        it "All times match" $ do
            go2 input `shouldBe` Just 1068781

go1 :: String -> Maybe Int
go1 = part1 day13 <=< parse day13

go2 :: String -> Maybe Int
go2 = part2 day13 <=< parse day13

input :: String
input =
    unlines
        [ "939"
        , "7,13,x,x,59,x,31,19"
        ]
