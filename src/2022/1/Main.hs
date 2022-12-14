module Main (main) where

import Utils (parseInt)
import Data.List.Split (splitOn)
import Data.List (sort)

type Elf = Int

readInputFile :: IO String
readInputFile = readFile "./input.txt"

main :: IO ()
main = do
  x <- readInputFile
  let parted = splitOn "\n\n" x
  let elves = map ((sum . map parseInt) . lines) parted
  print $ part1 elves
  print $ part2 elves

part1 :: [Elf] -> Int
part1 = maximum

part2 :: [Elf] -> Int
part2 elves = sumOfCalories
  where 
    sumOfCalories = sum $ take 3 $ (reverse . sort) elves