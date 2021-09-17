{-# LANGUAGE TupleSections #-}

module AOC.Day14 (day14) where

import Data.Bits (clearBit, complement, complementBit, setBit, zeroBits, (.&.), (.|.))
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Void (Void)
import Data.Word (Word64)
import Text.Megaparsec (Parsec, between, eof, many, parseMaybe, parseTest, (<|>))
import Text.Megaparsec.Char (char, digitChar, newline, string)

import AOC.Types

day14 :: Solution [MemBlock] Word64 Word64
day14 =
    Solution
        { parse = parseMaybe memBlocksP
        , part1 = Just . runProgram maskValue
        , part2 = Just . runProgram maskIndex
        }

type Parser = Parsec Void String

data MemBlock = MemBlock Mask [Mem]
    deriving (Show, Eq)

memBlocksP :: Parser [MemBlock]
memBlocksP = many memBlockP <* eof

memBlockP :: Parser MemBlock
memBlockP = MemBlock <$> maskP <*> many memP

data Mask = Mask Word64 Word64 [Int]
    deriving (Show, Eq)

mkMask :: [MaskChar] -> Mask
mkMask = mkMask . maskParts . bitPos
  where
    mkMask (z, o, x) = Mask z o x
    maskParts xs =
        ( complementBits (2 ^ 36 - 1 :: Word64) $ bitFilter Zero
        , complementBits zeroBits $ bitFilter One
        , bitFilter X
        )
      where
        complementBits w = foldl' complementBit w
        bitFilter target = map fst $ filter ((target ==) . snd) xs
    bitPos = zip [0 ..] . reverse

data MaskChar = Zero | One | X
    deriving (Show, Eq)

maskP :: Parser Mask
maskP = mkMask <$> (string "mask = " *> many maskCharP <* newline)
  where
    maskCharP = zero <|> one <|> x
    zero = Zero <$ char '0'
    one = One <$ char '1'
    x = X <$ char 'X'

data Mem = Mem Word64 Word64
    deriving (Show, Eq)

memP :: Parser Mem
memP =
    Mem
        <$> (string "mem" *> between (char '[') (char ']') word64P)
        <*> (string " = " *> word64P <* newline)

word64P :: Parser Word64
word64P = read <$> many digitChar

runProgram :: (Mem -> Mask -> Map Word64 Word64) -> [MemBlock] -> Word64
runProgram f = sum . Map.elems . foldl' (runMemBlock f) Map.empty

runMemBlock :: (Mem -> Mask -> Map Word64 Word64) -> Map.Map Word64 Word64 -> MemBlock -> Map.Map Word64 Word64
runMemBlock f memory (MemBlock mask ms) = foldl' accum memory ms
  where
    accum memMap mem = let memMap' = f mem mask in Map.union memMap' memMap

maskValue :: Mem -> Mask -> Map Word64 Word64
maskValue (Mem i m) (Mask z o _) = Map.fromList [(i, z .&. m .|. o)]

maskIndex :: Mem -> Mask -> Map Word64 Word64
maskIndex (Mem i m) (Mask _ o x) = Map.fromList . map (,m) $ foldl' accum [i .|. o] x
  where
    accum is n = concatMap (\x -> [x, complementBit x n]) is
