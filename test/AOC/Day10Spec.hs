module AOC.Day10Spec (spec) where

import Test.Hspec
import AOC (day10, parse, part1, part2)

spec :: Spec
spec = do
  describe "day10 part1" $ do
    it "joltage difference product" $ do
      go1 input `shouldBe` Just 220
  describe "day10 part2" $ do
    it "count adapter arrangements" $ do
      go2 input `shouldBe` Just 19208

go1 :: String -> Maybe Int
go1 = (part1 day10 =<<) . parse day10

go2 :: String -> Maybe Int
go2 = (part2 day10 =<<) . parse day10

input :: String
input = unlines
    [ "28"
    , "33"
    , "18"
    , "42"
    , "31"
    , "14"
    , "46"
    , "20"
    , "48"
    , "47"
    , "24"
    , "23"
    , "49"
    , "45"
    , "19"
    , "38"
    , "39"
    , "11"
    , "1"
    , "32"
    , "25"
    , "35"
    , "8"
    , "17"
    , "7"
    , "9"
    , "4"
    , "2"
    , "34"
    , "10"
    , "3"
    ]
