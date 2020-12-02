module AOC.Day01Spec (spec) where

import Test.Hspec
import AOC (day01, parse, part1, part2)

spec :: Spec
spec = do
  describe "day01 part1" $ do
    it "2-sum" $ do
      go1 "1721\n979\n366\n299\n675\n1456" `shouldBe` Just 514579
  describe "day01 part2" $ do
    it "3-sum" $ do
      go2 "1721\n979\n366\n299\n675\n1456" `shouldBe` Just 241861950

go1 :: String -> Maybe Int
go1 = (part1 day01 =<<) . parse day01

go2 :: String -> Maybe Int
go2 = (part2 day01 =<<) . parse day01
