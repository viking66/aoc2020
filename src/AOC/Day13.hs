{-# LANGUAGE BangPatterns #-}

module AOC.Day13 (day13) where

import Data.Function (on)
import Data.List (find, foldl', group)
import Data.List.Split (splitOn)
import Safe (maximumByMay, minimumMay, readMay)

import AOC.Types

day13 :: Solution Schedule Int Int
day13 =
    Solution
        { parse = toSchedule
        , part1 = part1'
        , part2 = part2'
        }

data Schedule = Schedule Int [Bus]

data Bus = Bus Int Int

toSchedule :: String -> Maybe Schedule
toSchedule = go . lines
  where
    go [t, s] = Schedule <$> readMay t <*> toBuses s
    go _ = Nothing
    toBuses = traverse toBus . filter (("x" /=) . snd) . zip [0 ..] . splitOn ","
    toBus (i, s) = Bus i <$> readMay s

part1' :: Schedule -> Maybe Int
part1' (Schedule t bs) = fmap (uncurry (*)) . minimumMay $ map f bs
  where
    f (Bus _ b) = (b - (t `mod` b), b)

-- I got stuck on this and didn't feel like learning the Chinese Remainder Theorem so I copied a solution from
-- https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day13.md
part2' :: Schedule -> Maybe Int
part2' (Schedule _ bs) = Just . fst $ foldl' go (0, 1) bs
  where
    go (!base, !step) (Bus i s) = (base', step * s)
      where
        base' = until (\n -> (n + i) `mod` s == 0) (+ step) base
