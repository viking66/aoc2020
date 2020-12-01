{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import AOC
import qualified Data.Map as Map
import System.Environment (getArgs)
import Text.Printf (printf)

aoc :: (Show b, Show c) => Int -> Solution a b c -> IO ()
aoc n Solution{..} = readFile filename >>= runSolution
  where
    filename = "data/" <> (printf "%02d" n) <> ".txt"
    runSolution input = case getResults input of
      Nothing -> putStrLn "bad input"
      Just (x, y) -> printResult x *> printResult y
    getResults = ((\i -> Just (part1 i, part2 i)) =<<) . parse
    printResult :: Show a => Maybe a -> IO ()
    printResult = putStrLn . maybe "something went wrong" show

solutions :: Map.Map Int (IO ())
solutions = Map.fromList
  [ (1, aoc 1 day01)
  , (2, aoc 2 day02)
  , (3, aoc 3 day03)
  , (4, aoc 4 day04)
  , (5, aoc 5 day05)
  , (6, aoc 6 day06)
  , (7, aoc 7 day07)
  , (8, aoc 8 day08)
  , (9, aoc 9 day09)
  , (10, aoc 10 day10)
  , (11, aoc 11 day11)
  , (12, aoc 12 day12)
  , (13, aoc 13 day13)
  , (14, aoc 14 day14)
  , (15, aoc 15 day15)
  ]

getArg :: IO (Maybe Int)
getArg = do
  args <- map read <$> getArgs
  pure $ case args of
    [n] -> Just n
    _ -> Nothing

main :: IO ()
main = do
  maybeN <- getArg
  case maybeN of
    Nothing -> putStrLn "expected single integer argument"
    Just n ->
      case Map.lookup n solutions of
        Nothing -> putStrLn $ "no sulution for " <> show n
        Just x -> x
