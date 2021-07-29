module AOC.Day09Spec (spec) where

import Test.Hspec
import AOC (XMAS, day09, mkXMAS, parse, part1, part2)

spec :: Spec
spec = do
  describe "day09 part1" $ do
    it "find missing sum number" $ do
      go1 input `shouldBe` Just 127
  describe "day09 part2" $ do
    it "find encryption weakness" $ do
      go2 input `shouldBe` Just 62

go1 :: XMAS -> Maybe Int
go1 = part1 day09

go2 :: XMAS -> Maybe Int
go2 = part2 day09

input :: XMAS
input = mkXMAS
    5 [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]
