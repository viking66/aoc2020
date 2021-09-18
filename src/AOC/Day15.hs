{-# LANGUAGE NamedFieldPuns #-}

module AOC.Day15 (day15) where

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Safe (initMay, lastMay, readMay)

import AOC.Types

day15 :: Solution GameState Int Int
day15 =
    Solution
        { parse = parseGameState
        , part1 = play 2020
        , part2 = play 30000000
        }

data GameState = GameState
    { number :: Int
    , index :: Int
    , history :: Map Int Int
    }
    deriving (Show)

parseGameState :: String -> Maybe GameState
parseGameState s = do
    xs <- traverse readMay $ splitOn "," s
    (xs', x) <- splitLast xs
    pure
        GameState
            { number = x
            , index = length xs
            , history = Map.fromList $ zip xs' [1 ..]
            }
  where
    splitLast xs = (,) <$> initMay xs <*> lastMay xs

play :: Int -> GameState -> Maybe Int
play n = nthNumber n . iterate nextNumber
  where
    nthNumber n = fmap number . find ((n ==) . index) . take n
    nextNumber GameState{number, index, history} =
        GameState
            { number = maybe 0 (index -) $ Map.lookup number history
            , index = succ index
            , history = Map.insert number index history
            }
