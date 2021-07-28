module AOC.Day08Spec (spec) where

import Test.Hspec

import AOC (day08, parse, part1, part2)

spec :: Spec
spec = do
  describe "day08 part1" $ do
    it "accumulator when console enters infinite loop" $ do
      go1 input `shouldBe` Just 5
  describe "day08 part2" $ do
    it "accumulator when repaired console halts" $ do
      go2 input `shouldBe` Just 8

go1 :: String -> Maybe Int
go1 = (part1 day08 =<<) . parse day08

go2 :: String -> Maybe Int
go2 = (part2 day08 =<<) . parse day08

input :: String
input = unlines
    [ "nop +0"
    , "acc +1"
    , "jmp +4"
    , "acc +3"
    , "jmp -3"
    , "acc -99"
    , "acc +1"
    , "jmp -4"
    , "acc +6"
    ]
