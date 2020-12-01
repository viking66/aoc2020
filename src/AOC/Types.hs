module AOC.Types
  ( Solution (..)
  ) where

data Solution a b c = Solution
  { parse :: String -> Maybe a
  , part1 :: a -> Maybe b
  , part2 :: a -> Maybe c
  }
