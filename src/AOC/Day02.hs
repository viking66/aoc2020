module AOC.Day02 (day02) where

import AOC.Types
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Safe (atMay)
import Text.Megaparsec (Parsec, many, parseMaybe)
import Text.Megaparsec.Char (char, letterChar, newline, space1)
import Text.Megaparsec.Char.Lexer (decimal)

day02 :: Solution [PasswordTest] Int Int
day02 = Solution
  { parse = parseMaybe passwordTestListP
  , part1 = Just . length . filter runPasswordCountTest
  , part2 = Just . length . filter runPasswordIndexTest
  }

data PasswordRule = PasswordRule Int Int Char
  deriving Show

data PasswordTest = PasswordTest PasswordRule String
  deriving Show

type Parser = Parsec Void String

intP :: Parser Int
intP = decimal

passwordRuleP :: Parser PasswordRule
passwordRuleP = PasswordRule <$> intP <* char '-' <*> intP <* space1 <*> letterChar <* char ':'

passwordTestP :: Parser PasswordTest
passwordTestP = PasswordTest <$> passwordRuleP <* space1 <*> many letterChar <* newline

passwordTestListP :: Parser [PasswordTest]
passwordTestListP = many passwordTestP

runPasswordCountTest :: PasswordTest -> Bool
runPasswordCountTest (PasswordTest (PasswordRule lo hi c) s) =
  let n = length $ filter (==c) s
  in lo <= n && n <= hi

runPasswordIndexTest :: PasswordTest -> Bool
runPasswordIndexTest (PasswordTest (PasswordRule lo hi c) s) = fromMaybe False $ do
  x <- atMay' lo
  y <- atMay' hi
  Just $ (x==c) /= (y==c)
  where
    atMay' = atMay s . pred
