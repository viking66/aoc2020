module AOC.Day01 (day01) where

import Safe (headMay, readMay)

import AOC.Types
day01 :: Solution [Int] Int Int
day01 = Solution
  { parse = traverse readMay . lines
  , part1 = \xs -> headMay [x*y | x <- xs, y <- xs, x+y==2020]
  , part2 = \xs -> headMay [x*y*z | x <- xs, y <- xs, z <- xs, x+y+z==2020]
  }
