module AOC.Day04 (day04) where

import AOC.Types

import Control.Monad (guard)
import Data.Char (toLower)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), count, eof, many, parseMaybe)
import Text.Megaparsec.Char (asciiChar, char, digitChar, hexDigitChar, letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

day04 :: Solution [Passport] Int Int
day04 = Solution
  { parse = Just . toPassports
  , part1 = Just . length
  , part2 = Just . length . filter validate
  }

data Passport = Passport
    { byr :: String
    , iyr :: String
    , eyr :: String
    , hgt :: String
    , hcl :: String
    , ecl :: String
    , pid :: String
    }
    deriving Show

type Parser = Parsec Void String

keyValP :: Parser (String, String)
keyValP = (,) <$> many letterChar <* char ':' <*> many asciiChar

toPassports :: String -> [Passport]
toPassports = mapMaybe (toPassport . Map.fromList)
    . mapMaybe (traverse (parseMaybe keyValP) . words)
    . splitOn "\n\n"
  where
    toPassport m = do
        byr' <- Map.lookup "byr" m
        iyr' <- Map.lookup "iyr" m
        eyr' <- Map.lookup "eyr" m
        hgt' <- Map.lookup "hgt" m
        hcl' <- Map.lookup "hcl" m
        ecl' <- Map.lookup "ecl" m
        pid' <- Map.lookup "pid" m
        pure $ Passport
            { byr = byr'
            , iyr = iyr'
            , eyr = eyr'
            , hgt = hgt'
            , hcl = hcl'
            , ecl = ecl'
            , pid = pid'
            }

validate :: Passport -> Bool
validate p = fromMaybe False $ do
    byr' <- parseMaybe intP $ byr p
    iyr' <- parseMaybe intP $ iyr p
    eyr' <- parseMaybe intP $ eyr p
    hgt' <- parseMaybe hgtP $ hgt p
    hcl' <- parseMaybe hclP $ hcl p
    ecl' <- parseMaybe eclP $ ecl p
    pid' <- parseMaybe pidP $ pid p
    pure $ byr' >= 1920 && byr' <= 2002
        && iyr' >= 2010 && iyr' <= 2020
        && eyr' >= 2020 && eyr' <= 2030
        && validHeight hgt'
        && hcl' == map toLower hcl'

intP :: Parser Int
intP = decimal <* eof

data Height = In Int | Cm Int

validHeight :: Height -> Bool
validHeight (In n) = n >= 59 && n <= 76
validHeight (Cm n) = n >= 150 && n <= 193

hgtP :: Parser Height
hgtP = f <$> decimal <*> (inP <|> cmP) <* eof
  where
    f n constructor = constructor n
    inP = In <$ string "in"
    cmP = Cm <$ string "cm"

hclP :: Parser String
hclP = char '#' *> count 6 hexDigitChar <* eof

eclP :: Parser String
eclP = (string "amb"
    <|> string "blu"
    <|> string "brn"
    <|> string "gry"
    <|> string "grn"
    <|> string "hzl"
    <|> string "oth") <* eof

pidP :: Parser String
pidP = count 9 digitChar <* eof
