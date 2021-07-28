module AOC.Day05Spec (spec) where

import Test.Hspec
import AOC (day05, parse, part1, part2)

spec :: Spec
spec = do
  describe "day05 part1" $ do
    it "calculate seat ID" $ do
      go1 "FBFBBFFRLR" `shouldBe` Just 357

go1 :: String -> Maybe Int
go1 = (part1 day05 =<<) . parse day05
