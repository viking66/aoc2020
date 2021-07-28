{-# LANGUAGE LambdaCase #-}
module AOC.Day08 (day08) where

import AOC.Types

import Control.Monad (join)
import Data.Bool (bool)
import Data.List (find)
import Data.List.Zipper (Zipper, fromList, left, replace, right, safeCursor)
import Safe (readMay)

day08 :: Solution [Instruction] Int Int
day08 = Solution
  { parse = traverse toInstruction . lines
  , part1 = Just . accumulator . runConsole . newConsole
  , part2 = fmap accumulator
          . find ((Halted ==) . getExecState)
          . map (runConsole . newConsole)
          . overrides
  }

data Instruction = NOP Int | ACC Int | JMP Int | HALT
    deriving (Show, Eq)

toInstruction :: String -> Maybe Instruction
toInstruction = toInst . words
  where
    toInst [inst, n] = op inst <*> num n
    toInst _ = Nothing
    op = \case
        "nop" -> Just NOP
        "acc" -> Just ACC
        "jmp" -> Just JMP
        _ -> Nothing
    num = \case
        ('+':n) -> readMay n
        ('-':n) -> negate <$> readMay n
        _ -> Nothing

data Console = Console
    { instructions :: Zipper (Maybe Instruction)
    , accumulator :: Int
    }
    deriving (Show, Eq)

data ExecState = Running | Halted | Error
    deriving (Show, Eq)

newConsole :: [Instruction] -> Console
newConsole is = Console
    { instructions = fromList . map Just $ is <> [HALT]
    , accumulator = 0
    }

getInst :: Console -> Maybe Instruction
getInst = join . safeCursor . instructions

getExecState :: Console -> ExecState
getExecState = go . getInst
  where
    go = \case
        Nothing -> Error
        Just HALT -> Halted
        _ -> Running

runConsole :: Console -> Console
runConsole = recur . execInst
  where
    running = (Running ==) . getExecState
    recur c = bool c (runConsole c) (running c)
    execInst console
        | running console = maybe console go $ getInst console
        | otherwise = console
      where
        go = \case
            NOP _ -> moveInstPtr 1 $ rmInst console
            ACC n -> moveInstPtr 1 $ addAccumulator n $ rmInst console
            JMP n -> moveInstPtr n $ rmInst console
            HALT  -> console
        rmInst console = console{instructions = replace Nothing (instructions console)}
        addAccumulator n console = console{accumulator = accumulator console + n}
        moveInstPtr n console
            | n > 0 = moveInstPtr (pred n) console{instructions = right (instructions console)}
            | n < 0 = moveInstPtr (succ n) console{instructions = left (instructions console)}
            | otherwise = console

overrides :: [Instruction] -> [[Instruction]]
overrides is = map (override' . fst) . filter nopOrJmp $ zip [0..] is
  where
    nopOrJmp (_, NOP _) = True
    nopOrJmp (_, JMP _) = True
    nopOrJmp _ = False
    override' n = case splitAt n is of
        (xs, y:ys) -> xs <> (toggle y:ys)
        _ -> is
    toggle = \case
        NOP n -> JMP n
        JMP n -> NOP n
        i -> i
