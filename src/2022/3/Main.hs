module Main (main) where

import Data.List (intersect)
import qualified Data.Set as Set
import Data.Char (ord, isUpper)
import Utils (halve)
import Data.List.Split (chunksOf)

type Rucksack = (String, String)

readInputFile :: IO String
readInputFile = readFile "./test.txt"

main :: IO ()
main = do
  inp <- readInputFile
  let parsed = lines inp
  print $ part1 parsed
  print $ part2 parsed


findDuplicateLetters :: Rucksack -> String
findDuplicateLetters (firstHalf, secondHalf) =
  Set.toList $ Set.fromList (firstHalf `intersect` secondHalf)

getPriority :: Char -> Int
getPriority char =
  if isUpper char then
    ord char - 38
  else
    ord char - 96

part1 :: [String] -> Int
part1 inp = sumOfPriorities
  where
    rucksacks = map halve inp
    duplicates = map (head . findDuplicateLetters) rucksacks
    sumOfPriorities = sum $ map getPriority duplicates

findCommonLetters :: [String] -> String
findCommonLetters rucksacks = Set.toList $ Set.fromList (foldr1 intersect rucksacks)

part2 :: [String] -> Int
part2 inp = sumOfPriorities
  where
    commons = map (head . findCommonLetters) (chunksOf 3 inp)
    sumOfPriorities = sum $ map getPriority commons
