module AOC.Day01Spec (spec) where

import Test.Hspec
import AOC (day01, parse, part1, part2)

spec :: Spec
spec = do
  describe "day01 ..." $ do
    it "no-op" $ do
      () `shouldBe` ()
