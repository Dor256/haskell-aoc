{-# LANGUAGE TupleSections #-}
module Main (main) where

import Utils (Point, Grid, up, down, left, right)
import Data.List (foldl', find, minimumBy)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Char (ord)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import qualified Data.Set as Set
import Data.Function (on)

readInputFile :: IO String
readInputFile = readFile "./input.txt"

parseGrid :: [(Int, [(Int, Char)])] -> Grid Char
parseGrid = foldl' buildGrid Map.empty
  where
    buildGrid grid row =
      foldl'
        (\acc x ->
          Map.insert (fst x, fst row) (snd x) acc
        )
        grid
        (snd row)

main :: IO ()
main = do
  inp <- readInputFile
  let rawGrid = map (map head . filter (not . null) . splitOn "") (lines inp)
  let grid = parseGrid $ zip [0..] $ map (zip [0..]) rawGrid
  print $ part1 grid
  print $ part2 grid

getAdjList :: Grid Char -> (Point, Char) -> [(Point, Char)]
getAdjList graph vertex =
  filter (\(_, char) -> isLegalPass vertex char) $
  catMaybes
  [ up graph (Just vertex)
  , down graph (Just vertex)
  , left graph (Just vertex)
  , right graph (Just vertex)
  ]
    where
      isLegalPass (_, current) adjVertex =
        if adjVertex == 'E' then
          current == 'z' || current == 'y'
        else
          ord adjVertex - ord (if current == 'S' then 'a' else current) <= 1

bfs :: ((Point, Char) -> [(Point, Char)]) -> (Point, Char) -> [((Point, Char), Int)]
bfs getNeighbors start = go (Set.singleton start) [(start, 0)]
  where
    go visited ((vertex, step):queue) = (vertex, step) : go newVisited newQueue
      where
        adjList = filter (`Set.notMember` visited) $ getNeighbors vertex
        newQueue = queue ++ map (, step + 1) adjList
        newVisited = foldl' (flip Set.insert) visited adjList
    go _ [] = []

part1 :: Grid Char -> Int
part1 grid =
  let
    source = head $ Map.toList $ Map.filter (=='S') grid
    sink = head $ Map.toList $ Map.filter (=='E') grid
  in
  snd $ fromJust $ find (\(point, _) -> point == sink) $ bfs (getAdjList grid) source

part2 :: Grid Char -> Int
part2 grid =
  let
    sources = Map.toList $ Map.filter (\height -> height =='a' || height =='S') grid
    sink = head $ Map.toList $ Map.filter (=='E') grid
  in
  snd $ minimumBy (compare `on` snd) $ mapMaybe (find (\(point, _) -> point == sink) . bfs (getAdjList grid)) sources
