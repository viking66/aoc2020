{-# LANGUAGE LambdaCase #-}

module AOC.Day12 (day12) where

import Data.List (foldl')
import Safe (readMay)

import AOC.Types

day12 :: Solution [Action] Int Int
day12 =
    Solution
        { parse = traverse parseAction . lines
        , part1 = Just . distance . foldl' (action updateShip) (newShip $ Coord 1 0)
        , part2 = Just . distance . foldl' (action updateWaypoint) (newShip $ Coord 10 1)
        }

data Action = N Int | S Int | E Int | W Int | T Int | F Int
    deriving (Show)

parseAction :: String -> Maybe Action
parseAction "" = Nothing
parseAction (x : xs) = action <*> readMay xs
  where
    action = case x of
        'N' -> Just N
        'S' -> Just S
        'E' -> Just E
        'W' -> Just W
        'L' -> Just $ T . (dirCount -) . turns
        'R' -> Just $ T . turns
        'F' -> Just F
        _ -> Nothing
    turns = (`mod` dirCount) . (`div` 90)

data Direction = North | East | South | West
    deriving (Show, Eq, Ord, Enum, Bounded)

dirCount :: Int
dirCount = succ $ fromEnum (maxBound :: Direction)

data Coord = Coord Int Int
    deriving (Show)

instance Semigroup Coord where
    Coord a b <> Coord c d = Coord (a + c) (b + d)

scale :: Coord -> Int -> Coord
scale (Coord x y) n = Coord (x * n) (y * n)

rotate :: Int -> Coord -> Coord
rotate n c@(Coord x y)
    | n <= 0 = c
    | otherwise = rotate (pred n) (Coord y (- x))

data Ship = Ship Coord Coord
    deriving (Show)

newShip :: Coord -> Ship
newShip waypoint = Ship waypoint (Coord 0 0)

distance :: Ship -> Int
distance (Ship _ (Coord x y)) = abs x + abs y

updateWaypoint :: Ship -> Coord -> Ship
updateWaypoint (Ship waypoint ship) coord = Ship (waypoint <> coord) ship

updateShip :: Ship -> Coord -> Ship
updateShip (Ship waypoint ship) coord = Ship waypoint (ship <> coord)

action :: (Ship -> Coord -> Ship) -> Ship -> Action -> Ship
action f s@(Ship waypoint ship) = \case
    N n -> f s (Coord 0 n)
    S n -> f s (Coord 0 (- n))
    E n -> f s (Coord n 0)
    W n -> f s (Coord (- n) 0)
    T n -> Ship (rotate n waypoint) ship
    F n -> Ship waypoint (ship <> scale waypoint n)
