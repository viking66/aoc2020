module AOC.Day02Spec (spec) where

import Control.Monad ((<=<))
import Test.Hspec

import AOC (day02, parse, part1, part2)

spec :: Spec
spec = do
    describe "day02 part1" $ do
        it "password test char count" $ do
            go1 "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n" `shouldBe` Just 2
    describe "day02 part2" $ do
        it "password test index match" $ do
            go2 "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n" `shouldBe` Just 1

go1 :: String -> Maybe Int
go1 = part1 day02 <=< parse day02

go2 :: String -> Maybe Int
go2 = part2 day02 <=< parse day02
