{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Main (main) where

import qualified Data.Map as Map
import Data.List (foldl', tails)
import Data.Text (Text, pack)
import qualified Data.Text as T (lines, stripPrefix)
import qualified Data.Text.Read as T (decimal)

readInputFile :: IO String
readInputFile = readFile "./input.txt"

addSizeToDirs :: Int -> Map.Map [Text] Int -> [Text] -> Map.Map [Text] Int
addSizeToDirs fileSize tree directory = Map.insertWith (+) directory fileSize tree

parseDirectoryTree :: [Text] -> Map.Map [Text] Int
parseDirectoryTree = snd . foldl' step ([], Map.singleton [] 0)
  where
    step (_, dirs) "$ cd /" = ([], dirs)
    step (_:cwd, dirs) "$ cd .." = (cwd, dirs)
    step (cwd, dirs) (T.stripPrefix "$ cd " -> Just dir) = (dir:cwd, dirs)
    step (cwd, dirs) (T.stripPrefix "dir " -> Just dir) = (cwd, Map.insert (dir:cwd) 0 dirs)
    step (cwd, dirs) (T.decimal -> Right (fileSize, _)) =
        (cwd, foldl' (addSizeToDirs fileSize) dirs $ tails cwd)
    step state _ = state

main :: IO ()
main = do
  inp <- readInputFile
  let directoryTree = parseDirectoryTree $ T.lines $ pack inp
  print $ part1 directoryTree
  print $ part2 directoryTree

part1 :: Map.Map [Text] Int -> Int
part1 = sum . filter (<= 100000) . Map.elems

part2 :: Map.Map [Text] Int -> Int
part2 dirTree = res
  where
    dirSizes = Map.elems dirTree
    spaceNeeded = 30000000 - (70000000 - maximum dirSizes)
    res = minimum $ filter (>= spaceNeeded) dirSizes
