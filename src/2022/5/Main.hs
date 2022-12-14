module Main (main) where

import Data.List.Split
import Data.Char (isAlpha)
import Utils (mapTuple3, parseInt, fst3, snd3, thd3)
import qualified Data.Map as Map

data Crate = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z deriving (Show)

getTestMap :: Map.Map Int [Crate]
getTestMap = Map.fromList [(1, [N, Z]), (2, [D, C, M]), (3, [P])]

getInputMap :: Map.Map Int [Crate]
getInputMap = Map.fromList
  [
    (1, [F, L, M, W])
  , (2, [F, M, V, Z, B])
  , (3, [Q, L, S, R, V, H])
  , (4, [J, T, M, P, Q, V, S, F])
  , (5, [W, S, L])
  , (6, [W, J, R, M, P, V, F])
  , (7, [F, R, N, P, C, Q, J])
  , (8, [B, R, W, Z, S, P, H, V])
  , (9, [W, Z, H, G, C, J, M, B])
  ]

readInputFile :: IO String
readInputFile = readFile "./input.txt"

parseCommands :: [String] -> (String, String, String)
parseCommands [a, b, c] = (a, b, c)

main :: IO ()
main = do
  inp <- readInputFile
  let rawCommands = lines . unlines . drop 1 $ splitOn "\n\n" inp
  let commands = map (mapTuple3 parseInt . parseCommands . words . filter (not . isAlpha)) rawCommands
  print $ part1 commands getInputMap
  print $ part2 commands getInputMap

executeCommand :: [(Int, Int ,Int)] -> Map.Map Int [Crate] -> Map.Map Int [Crate]
executeCommand commands crates =
  let
    currentCommand = head commands
    amount = fst3 currentCommand
    src = snd3 currentCommand
    dest = thd3 currentCommand
    updateFn key val
      | key == dest = Just ((reverse . take amount) (Map.findWithDefault [] src crates) ++ val)
      | key == src = Just (drop amount val)
      | otherwise = Just val
    firstUpdate = Map.updateWithKey updateFn dest crates
    secondUpdate = Map.updateWithKey updateFn src firstUpdate
  in
  if null commands then
    crates
  else
    executeCommand (tail commands) secondUpdate

part1 :: [(Int, Int, Int)] -> Map.Map Int [Crate] -> [Crate]
part1 commands input = Map.elems $ Map.map head (executeCommand commands input)

executeCommand' :: [(Int, Int ,Int)] -> Map.Map Int [Crate] -> Map.Map Int [Crate]
executeCommand' commands crates =
  let
    currentCommand = head commands
    amount = fst3 currentCommand
    src = snd3 currentCommand
    dest = thd3 currentCommand
    updateFn key val
      | key == dest = Just (take amount (Map.findWithDefault [] src crates) ++ val)
      | key == src = Just (drop amount val)
      | otherwise = Just val
    firstUpdate = Map.updateWithKey updateFn dest crates
    secondUpdate = Map.updateWithKey updateFn src firstUpdate
  in
  if null commands then
    crates
  else
    executeCommand' (tail commands) secondUpdate

part2 :: [(Int, Int, Int)] -> Map.Map Int [Crate] -> [Crate]
part2 commands input = Map.elems $ Map.map head (executeCommand' commands input)
