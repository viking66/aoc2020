module AOC.Day06 (day06) where

import AOC.Types

import Data.Bifunctor (bimap)
import Data.Foldable (foldl')
import Data.List (span)
import Data.Set (Set)
import qualified Data.Set as Set

day06 :: Solution [[Set Char]] Int Int
day06 =
    Solution
        { parse = Just . parseInput
        , part1 = Just . groupSum Set.union
        , part2 = Just . groupSum Set.intersection
        }

parseInput :: String -> [[Set Char]]
parseInput = toSets . lines
  where
    toSets = toSets' . dropWhile null
    toSets' [] = []
    toSets' xs = uncurry (:) . bimap (map Set.fromList) toSets . span (not . null) $ xs

groupSum :: Ord a => (Set a -> Set a -> Set a) -> [[Set a]] -> Int
groupSum f = sum . map (Set.size . setMerge)
  where
    setMerge [] = mempty
    setMerge (x : xs) = foldl' f x xs
