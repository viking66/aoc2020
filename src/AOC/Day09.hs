{-# LANGUAGE NamedFieldPuns #-}
module AOC.Day09 (XMAS, day09, mkXMAS) where

import AOC.Types

import Control.Monad ((<=<))
import Data.List (find, tails, scanl')
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Safe (headMay, maximumMay, minimumMay, readMay)

day09 :: Solution XMAS Int Int
day09 = Solution
  { parse = fmap (mkXMAS 25) . traverse readMay . lines
  , part1 = missingSum
  , part2 = \xmas -> (`encryptionWeakness` nums xmas) =<< missingSum xmas
  }

data XMAS = XMAS
    { operands :: MultiSet Int
    , nums :: [Int]
    , targets :: [Int]
    }

mkXMAS :: Int -> [Int] -> XMAS
mkXMAS n ns = XMAS
    { operands = MultiSet.fromList xs
    , nums = ns
    , targets = ys
    }
  where
    (xs, ys) = splitAt n ns

nextTarget :: XMAS -> XMAS
nextTarget xmas@XMAS{operands, nums, targets}
    | null targets = xmas
    | null nums = XMAS{operands = operands, nums = nums, targets = targets'}
    | otherwise =
        let (operands', nums') = shift operands nums
        in XMAS{operands = operands', nums = nums', targets = targets'}
  where
    targets' = drop 1 targets
    shift o [] = (o, [])
    shift o (n:ns) = (addTarget $ MultiSet.delete n o, ns)
    addTarget o = maybe o (`MultiSet.insert` o) (headMay targets)

missingSum :: XMAS -> Maybe Int
missingSum xmas@XMAS{operands, nums, targets} = case targets of
    [] -> Nothing
    (t:ts) -> if hasPairSum t operands
              then missingSum (nextTarget xmas)
              else Just t

hasPairSum :: Int -> MultiSet Int -> Bool
hasPairSum target nums = any f (MultiSet.distinctElems nums)
  where
    f n = let m = target - n in MultiSet.member m nums && n /= m

encryptionWeakness :: Int -> [Int] -> Maybe Int
encryptionWeakness n = minMaxSum . trim <=< prefixSum
  where
    prefixSum = find targetSum . map (drop 1 . scanl' sums (0,0)) . tails
    sums (_, s) m = (m, s+m)
    targetSum = elem n . map snd
    trim = map fst . takeWhile ((<=n) . snd)
    minMaxSum xs = (+) <$> minimumMay xs <*> maximumMay xs
