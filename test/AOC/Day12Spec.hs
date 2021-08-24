module AOC.Day12Spec (spec) where

import Control.Monad ((<=<))
import Test.Hspec

import AOC (day12, parse, part1, part2)

spec :: Spec
spec = do
    describe "day12 part1" $ do
        it "Move ship" $ do
            go1 input `shouldBe` Just 25
    describe "day12 part2" $ do
        it "Move waypoint" $ do
            go2 input `shouldBe` Just 286

go1 :: String -> Maybe Int
go1 = part1 day12 <=< parse day12

go2 :: String -> Maybe Int
go2 = part2 day12 <=< parse day12

input :: String
input =
    unlines
        [ "F10"
        , "N3"
        , "F7"
        , "R90"
        , "F11"
        ]
