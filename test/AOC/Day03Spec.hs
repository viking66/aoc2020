module AOC.Day03Spec (spec) where

import Test.Hspec
import AOC (day03, parse, part1, part2)

spec :: Spec
spec = do
  describe "day03 part1" $ do
    it "tree count" $ do
      go1 input `shouldBe` Just 7
    it "tree product" $ do
      go2 input `shouldBe` Just 336

go1 :: String -> Maybe Int
go1 = (part1 day03 =<<) . parse day03

go2 :: String -> Maybe Int
go2 = (part2 day03 =<<) . parse day03

input :: String
input = unlines
  [ "..##......."
  , "#...#...#.."
  , ".#....#..#."
  , "..#.#...#.#"
  , ".#...##..#."
  , "..#.##....."
  , ".#.#.#....#"
  , ".#........#"
  , "#.##...#..."
  , "#...##....#"
  , ".#..#...#.#"
  ]
