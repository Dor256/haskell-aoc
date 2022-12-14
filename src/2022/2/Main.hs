module Main (main) where

import Utils (next, prev, fromEnum')
import Data.List.Split (splitOn)

type Round = (Choice, Choice)

data Choice = Rock | Paper | Scissors deriving (Show, Enum, Bounded, Eq)

instance Ord Choice where
  compare Rock Rock = EQ
  compare Rock Paper = LT
  compare Rock Scissors = GT
  compare Paper Paper = EQ
  compare Paper Rock = GT
  compare Paper Scissors = LT
  compare Scissors Scissors = EQ
  compare Scissors Paper = GT
  compare Scissors Rock = LT
  
stringToChoice :: String -> Choice
stringToChoice choice =
  case choice of
    "A" -> Rock
    "X" -> Rock
    "B" -> Paper
    "Y" -> Paper
    "C" -> Scissors
    "Z" -> Scissors
    _ -> undefined

readInputFile :: IO String
readInputFile = readFile "./input.txt"

main :: IO ()
main = do
  inp <- readInputFile
  let rounds = lines inp
  print $ part1 rounds
  print $ part2 rounds

getRounds :: [String] -> Round
getRounds [] = undefined
getRounds (elf:me)
  | length me > 1 = undefined
  | otherwise = (stringToChoice elf, stringToChoice (head me))

settleRound :: Round -> Int
settleRound (elf, me) = outcomeScore + fromEnum' me
  where
    outcomeScore = case compare elf me of
      GT -> 0
      EQ -> 3
      LT -> 6


part1 :: [String] -> Int
part1 inp = sum $ map settleRound rounds
  where
    rounds = map (getRounds . splitOn " ") inp

data Outcome = Win | Draw | Lose deriving (Show, Enum)

type Round2 = (Choice, Outcome)

stringToOutcome :: String -> Outcome
stringToOutcome outcome =
  case outcome of
    "X" -> Lose
    "Y" -> Draw
    "Z" -> Win
    _ -> undefined

getPart2Rounds :: [String] -> Round2
getPart2Rounds [] = undefined
getPart2Rounds (elf:me)
  | length me > 1 = undefined
  | otherwise = (stringToChoice elf, stringToOutcome (head me))

settlePart2Round :: Round2 -> Int
settlePart2Round (elf, outcome) = 
  case outcome of
    Win -> (fromEnum' . next) elf + 6
    Draw -> fromEnum' elf + 3
    Lose -> (fromEnum' . prev) elf + 0


part2 :: [String] -> Int
part2 inp = sum $ map settlePart2Round rounds
  where
    rounds = map (getPart2Rounds . splitOn " ") inp