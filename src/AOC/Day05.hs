{-# LANGUAGE LambdaCase #-}
module AOC.Day05 (day05) where

import AOC.Types

import Data.Foldable (foldl')
import Data.List (sort)

day05 :: Solution [Int] Int Int
day05 = Solution
  { parse = fmap (map (seatId . foldl' half allSeats))
        . traverse (traverse toSelector)
        . lines
  , part1 = Just . maximum
  , part2 = findGap . sort
  }

data Selector = F | B | L | R

data Range = Range Int Int

data SeatLocation = SeatLocation Range Range

data Direction = Lower | Upper

half :: SeatLocation -> Selector -> SeatLocation
half (SeatLocation row col) = \case
    F -> SeatLocation (half' Lower row) col
    B -> SeatLocation (half' Upper row) col
    L -> SeatLocation row (half' Lower col)
    R -> SeatLocation row (half' Upper col)
  where
    half' dir (Range x y) = case dir of
        Lower -> Range x mid
        Upper -> Range (succ mid) y
      where
        mid = (x+y) `div` 2

allSeats :: SeatLocation
allSeats = SeatLocation (Range 0 127) (Range 0 7)

toSelector :: Char -> Maybe Selector
toSelector = \case
    'F' -> Just F
    'B' -> Just B
    'L' -> Just L
    'R' -> Just R
    _ -> Nothing

seatId :: SeatLocation -> Int
seatId (SeatLocation (Range r _) (Range c _)) = r * 8 + c

findGap :: [Int] -> Maybe Int
findGap xs = foldr f Nothing $ zip xs (drop 1 xs)
  where
    f (a, b) seat
        | a + 2 == b = Just $ a + 1
        | otherwise = seat
