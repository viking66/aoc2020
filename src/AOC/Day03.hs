{-# LANGUAGE LambdaCase #-}
module AOC.Day03 (day03) where

import AOC.Types
import Data.List (transpose)
import Safe (atMay)

day03 :: Solution [[Content]] Int Int
day03 = Solution
  { parse = traverse (traverse parseContent) . lines
  , part1 = treeProduct [(3, 1)]
  , part2 = treeProduct [(1,1), (3,1), (5,1), (7,1), (1,2)]
  }

data Content = Empty | Tree
  deriving Show

parseContent :: Char -> Maybe Content
parseContent = \case
  '.' -> Just Empty
  '#' -> Just Tree
  _   -> Nothing

treeProduct :: [(Int, Int)] -> [[Content]] -> Maybe Int
treeProduct slopes grid = fmap product . traverse slopeTreeCount $ slopes
  where
    slopeTreeCount :: (Int, Int) -> Maybe Int
    slopeTreeCount (x, y) = slopeTreeCount' (cycle $ transpose grid) x (iterate (y+) y) 0

    slopeTreeCount' :: [[Content]] -> Int -> [Int] -> Int -> Maybe Int
    slopeTreeCount' _ _ [] _ = Nothing
    slopeTreeCount' grid x (y:ys) n =
      case splitAt x grid of
        (_, a:as) ->
          case atMay a y of
            Nothing -> Just n
            Just content -> slopeTreeCount' (a:as) x ys (treeIncrementer content n)
        _         -> Nothing

    treeIncrementer :: Content -> (Int -> Int)
    treeIncrementer = \case
      Empty -> id
      Tree  -> succ
