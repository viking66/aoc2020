module AOC.Day14Spec (spec) where

import Control.Monad ((<=<))
import Data.Word (Word64)
import Test.Hspec

import AOC (day14, parse, part1, part2)

spec :: Spec
spec = do
    describe "day14 part1" $ do
        it "Masked values" $ do
            go1 input1 `shouldBe` Just 165
    describe "Masked indexes" $ do
        it "All times match" $ do
            go2 input2 `shouldBe` Just 208

go1 :: String -> Maybe Word64
go1 = part1 day14 <=< parse day14

go2 :: String -> Maybe Word64
go2 = part2 day14 <=< parse day14

input1 :: String
input1 =
    unlines
        [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
        , "mem[8] = 11"
        , "mem[7] = 101"
        , "mem[8] = 0"
        ]

input2 :: String
input2 =
    unlines
        [ "mask = 000000000000000000000000000000X1001X"
        , "mem[42] = 100"
        , "mask = 00000000000000000000000000000000X0XX"
        , "mem[26] = 1"
        ]
