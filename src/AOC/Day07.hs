module AOC.Day07 (day07) where

import AOC.Types

import Control.Monad.State (State, get, modify)
import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, parseMaybe, parseTest, sepBy, (<|>))
import Text.Megaparsec.Char (char, letterChar, newline, spaceChar, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Function.Memoize (memoize)

day07 :: Solution RuleMap Int Int
day07 =
    Solution
        { parse = parseMaybe ruleMapP
        , part1 = Just . numContainingTarget target
        , part2 = Just . totalBagsInTarget target
        }

data Bag = Bag String String
    deriving (Show, Eq, Ord)

unBag :: Bag -> (String, String)
unBag (Bag s t) = (s, t)

target :: Bag
target = Bag "shiny" "gold"

data Rules = Rules Bag (MultiSet Bag)

newtype RuleMap = RuleMap (Map Bag (MultiSet Bag))
    deriving (Show)

type Parser = Parsec Void String

ruleMapP :: Parser RuleMap
ruleMapP = RuleMap . Map.fromList . map unwrap <$> many rulesP
  where
    unwrap (Rules bag set) = (bag, set)

rulesP :: Parser Rules
rulesP = Rules <$> bagP <* string "bags contain " <*> bagSetP <* char '.' <* newline

bagP :: Parser Bag
bagP = Bag <$> many letterChar <* spaceChar <*> many letterChar <* spaceChar

bagSetP :: Parser (MultiSet Bag)
bagSetP = MultiSet.fromList <$> (emptySetP <|> nonEmptySetP)
  where
    emptySetP = string "no other bags" $> []
    nonEmptySetP = concat <$> ruleP `sepBy` string ", "
    ruleP = replicate <$> decimal <* spaceChar <*> bagP <* (string "bags" <|> string "bag")

numContainingTarget :: Bag -> RuleMap -> Int
numContainingTarget target (RuleMap m) = pred . length . filter id . map go $ Map.keys m
  where
    go = memoize go' . unBag
    go' (s, t) = bag == target || isNested
      where
        bag = Bag s t
        isNested = any go . MultiSet.distinctElems . fromMaybe mempty $ Map.lookup bag m

totalBagsInTarget :: Bag -> RuleMap -> Int
totalBagsInTarget target (RuleMap m) = pred . memoize go $ unBag target
  where
    go (s,t) = succ . maybe 0 recurSet $ Map.lookup (Bag s t) m
    recurSet = sum . map (go . unBag) . MultiSet.toList
