{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Data.List (find, stripPrefix, groupBy)
import Utils (parseInt)
import Text.Printf (printf)

readInputFile :: IO String
readInputFile = readFile "./input.txt"

cpu :: [String] -> [(Int, Int)]
cpu = foldl step [(1, 1)]
  where
    step history "noop" = history ++ [(fst (last history) + 1, snd $ last history)]
    step history (stripPrefix "addx " -> Just toAdd) =
      history ++
      [ (fst (last history) + 1, snd (last history))
      , (fst (last history) + 2, snd (last history) + parseInt toAdd)
      ]
    step state _ = state

main :: IO ()
main = do
  inp <- readInputFile
  print $ part1 $ lines inp
  printf $ part2 $ lines inp

getSplitCycles :: Int -> [(Int, Int)] -> Int
getSplitCycles cyc history =
  case find ((==cyc) . fst) history of
    Just (currentCycle, value) -> getSplitCycles (cyc + 40) history + (currentCycle * value)
    Nothing -> 0

part1 :: [String] -> Int
part1 = getSplitCycles 20 . cpu

executeCRT :: [(Int, Int)] -> String
executeCRT = map
  (\(cyc, val) -> 
    if abs (cyc `mod` 40 - val) <= 1 then 
      '#'
    else
      '.'
  )

part2 :: [String] -> String
part2 inp = message
  where
    normalizedCycles = map (\(first, second) -> (first - 1, second)) $ cpu inp
    groups = groupBy (\(first1, _) (first2, _) -> (first1 `quot` 40) == (first2 `quot` 40)) normalizedCycles
    message = unlines $ map executeCRT groups
