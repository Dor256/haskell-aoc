module Main (main) where

import Data.List.Split (splitOn)
import Data.List (foldl')
import Utils (parseInt, Grid, Point(..), up, left, right, down)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, catMaybes)

readInputFile :: IO String
readInputFile = readFile "./input.txt"

parse :: [(Int, [(Int, Int)])] -> Grid Int
parse = foldl' buildGrid Map.empty
  where
    buildGrid grid row =
      foldl'
        (\acc x ->
          Map.insert (fst x, fst row) (snd x) acc
        )
        grid
        (snd row)

listPointsInDir :: (Grid a -> Maybe (Point, a) -> Maybe (Point, a)) -> Maybe (Point, a) -> Grid a -> [(Point, a)]
listPointsInDir dirFn maybeElem grid = 
  catMaybes $ takeWhile isJust $ drop 1 $ iterate (dirFn grid) maybeElem

main :: IO ()
main = do
  inp <- readInputFile
  let rawGrid = map (map parseInt . (filter (not . null) . splitOn "")) (lines inp)
  let grid = parse $ zip [0..] $ map (zip [0..]) rawGrid
  print $ part1 grid
  print $ part2 grid

isVisible :: Grid Int -> Maybe (Point, Int) -> Bool
isVisible grid tree =
  let
    treeHeight = snd $ fromJust tree
  in
  all ((<treeHeight) . snd) (listPointsInDir up tree grid)
  || all ((<treeHeight) . snd) (listPointsInDir down tree grid)
  || all ((<treeHeight) . snd) (listPointsInDir left tree grid)
  || all ((<treeHeight) . snd) (listPointsInDir right tree grid)


part1 :: Grid Int -> Int
part1 grid =
  Map.foldlWithKey
    (\acc point height ->
      if isVisible grid (Just (point, height)) then
        acc + 1
      else
        acc
    )
    0
    grid

countScenicScore :: Grid Int -> Maybe (Point, Int) -> Int
countScenicScore grid tree =
  let
    treeHeight = snd $ fromJust tree
    breakOnBlock = foldl' (\acc curr -> if null acc || last acc < treeHeight then acc ++ [curr] else acc) [] 
    upScore = length $ breakOnBlock $ map snd (listPointsInDir up tree grid)
    downScore = length $ breakOnBlock $ map snd (listPointsInDir down tree grid)
    leftScore = length $ breakOnBlock $ map snd (listPointsInDir left tree grid)
    rightScore = length $ breakOnBlock $ map snd (listPointsInDir right tree grid)
  in
  upScore * downScore * leftScore * rightScore

gridMaxPoint :: Grid Int -> Point
gridMaxPoint = fst . Map.findMax

gridMinPoint :: Grid Int -> Point
gridMinPoint = fst . Map.findMin

isGridEdge :: Grid Int -> Point -> Int -> Bool
isGridEdge grid (x, y) _
  | x ==  fst (gridMaxPoint grid) = True
  | y == snd (gridMaxPoint grid) = True
  | y == snd (gridMinPoint grid) = True
  | x == fst (gridMinPoint grid) = True
  | otherwise = False

part2 :: Grid Int -> Int
part2 grid =
  maximum $ Map.foldlWithKey
    (\scores point height ->
      if isGridEdge grid point height then
        scores
      else
        countScenicScore grid (Just (point, height)):scores
    )
    []
    grid