{-# LANGUAGE TupleSections #-}
module AOC.Day10 (day10) where

import AOC.Types

import Data.Bifunctor (bimap)
import Data.Function.Memoize (memoFix)
import Data.List (group, sort)
import Safe (readMay)

day10 :: Solution [Int] Int Int
day10 = Solution
  { parse = fmap groupAdaptors . traverse readMay . lines
  , part1 = \xs -> Just $ sum xs * (length xs - 1)
  , part2 = Just . product . map fib3
  }

groupAdaptors :: [Int] -> [Int]
groupAdaptors = map (pred . length) . runs . extend . sort
  where
    extend [] = []
    extend xs = 0:xs<>[3 + maximum xs]

    runs = takeWhile (not . null) . map fst . drop 1 . iterate (splitRun . snd) . ([],)
      where
        splitRun [] = ([], [])
        splitRun xss@(x:xs) = bimap (map fst) (map fst) . span (uncurry (==)) $ zip xss [x..]

fib3 :: Int -> Int
fib3 = memoFix go
  where
    go f n
        | n < 2 = 1
        | n == 2 = 2
        | otherwise = f (n-1) + f (n-2) + f (n-3)
