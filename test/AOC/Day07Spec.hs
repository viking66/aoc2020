module AOC.Day07Spec (spec) where

import Control.Monad ((<=<))
import Test.Hspec

import AOC (day07, parse, part1, part2)

spec :: Spec
spec = do
    describe "day07 part1" $ do
        it "number of bags containing shiny gold" $ do
            go1 input `shouldBe` Just 4
    describe "day07 part2" $ do
        it "number of bags inside shiny gold" $ do
            go2 input `shouldBe` Just 32

go1 :: String -> Maybe Int
go1 = part1 day07 <=< parse day07

go2 :: String -> Maybe Int
go2 = part2 day07 <=< parse day07

input :: String
input =
    unlines
        [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
        , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
        , "bright white bags contain 1 shiny gold bag."
        , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
        , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
        , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
        , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
        , "faded blue bags contain no other bags."
        , "dotted black bags contain no other bags."
        ]
