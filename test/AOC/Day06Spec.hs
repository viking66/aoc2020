module AOC.Day06Spec (spec) where

import Control.Monad ((<=<))
import Test.Hspec

import AOC (day06, parse, part1, part2)

spec :: Spec
spec = do
    describe "day06 part1" $ do
        it "count of anyone who said yes" $ do
            go1 input `shouldBe` Just 11
    describe "day06 part2" $ do
        it "count where everyone said yes" $ do
            go2 input `shouldBe` Just 6

go1 :: String -> Maybe Int
go1 = part1 day06 <=< parse day06

go2 :: String -> Maybe Int
go2 = part2 day06 <=< parse day06

input :: String
input =
    unlines
        [ "abc"
        , ""
        , "a"
        , "b"
        , "c"
        , ""
        , "ab"
        , "ac"
        , ""
        , "a"
        , "a"
        , "a"
        , "a"
        , ""
        , "b"
        ]
