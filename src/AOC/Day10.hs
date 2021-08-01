{-# LANGUAGE TupleSections #-}
module AOC.Day10 (day10) where

import AOC.Types

import Data.Bifunctor (bimap)
import Data.Function.Memoize (memoFix)
import Data.List (group, sort)
import Safe (readMay)

day10 :: Solution [Int] Int Int
day10 = Solution
  { parse = fmap sort . traverse readMay . lines
  , part1 = Just . joltageDiffProduct
  , part2 = Just . countAdapterArrangements
  }

joltageDiffProduct :: [Int] -> Int
joltageDiffProduct = go
  where
    go [] = 0
    go xss@(_:xs) = product . map (succ . length) . group . sort $ zipWith (-) xs xss

countAdapterArrangements :: [Int] -> Int
countAdapterArrangements = product . map (fib3 . pred . length) . runs . extend
  where
    extend [] = []
    extend xs = 0:xs<>[3 + maximum xs]

    runs = takeWhile (not . null) . map fst . drop 1 . iterate (splitRun . snd) . ([],)
      where
        splitRun [] = ([], [])
        splitRun xss@(x:xs) = bimap (map fst) (map fst) . span (uncurry (==)) $ zip xss [x..]

    fib3 = memoFix go
      where
        go _ 0 = 1
        go _ 1 = 1
        go _ 2 = 2
        go f n = f (n-1) + f (n-2) + f (n-3)
