module Main (main) where

import qualified Text.Parsec as Parsec
import Utils (parseInt, Point(..))
import Data.Either (rights)
import Data.List (foldl', group, sort)

data Direction = U | D | L | R deriving (Show, Eq)

up :: Point -> Point
up (x, y) = (x, y + 1)

down :: Point -> Point
down (x, y) = (x, y - 1)

left :: Point -> Point
left (x, y) = (x - 1, y)

right :: Point -> Point
right (x, y) = (x + 1, y)

toDirection :: Char -> Direction
toDirection 'U' = U
toDirection 'D' = D
toDirection 'L' = L
toDirection 'R' = R
toDirection _ = undefined

readInputFile :: IO String
readInputFile = readFile "./test2.txt"

commandParser :: Parsec.Parsec String () (Direction, Int)
commandParser = do
  dir <- Parsec.anyChar
  _ <- Parsec.space
  amount <- Parsec.many1 Parsec.digit
  return (toDirection dir, parseInt amount)

move :: Direction -> (Point -> Point)
move dir
  | dir == U = up
  | dir == D = down
  | dir == L = left
  | dir == R = right
  | otherwise = undefined

executeCommands :: [(Direction, Int)] -> [Point]
executeCommands = foldl' step [(0, 0)]
  where
    step history (dir, amount) =
      init history ++ take (amount + 1) (iterate (move dir) (last history))

follow :: Point -> Point -> Point
follow tl@(tlx, tly) (hdx, hdy) = res
  where
    xDiff = hdx - tlx
    yDiff = hdy - tly

    xIsFar = abs xDiff > 1
    yIsFar = abs yDiff > 1
    isNotFar = not (xIsFar || yIsFar)

    newX = if abs xDiff > 1 then tlx + xDiff + (if xDiff < 0 then 1 else -1) else hdx
    newY = if abs yDiff > 1 then tly + yDiff + (if yDiff < 0 then 1 else -1) else hdy
    res = if isNotFar then tl else (newX, newY)


main :: IO ()
main = do
  inp <- readInputFile
  let commands = rights $ map (Parsec.parse commandParser "") (lines inp)
  let history = executeCommands commands
  print $ part1 history
  print $ part2 history

getFollowPoints :: [Point] -> [Point]
getFollowPoints history =
  let
    initialTail = head history
  in
  foldl (\acc headLoc -> acc ++ [follow (last acc) headLoc]) [initialTail] history

part1 :: [Point] -> Int
part1 history = length $ group $ sort $ getFollowPoints history

part2 :: [Point] -> Int
part2 history =
  let
    allTails = replicate 9 "ignore"
  in
  length $ group $ sort $ foldl (\prevTail _ -> getFollowPoints prevTail) history allTails