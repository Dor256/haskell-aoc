module Main (main) where

import qualified Text.Parsec as Parsec
import Utils (Point, Grid, parseInt)
import Data.Either (fromRight)
import qualified Data.Map as Map
import Data.Ix (Ix(range))
import Data.Maybe (fromMaybe)

down :: Point -> Point
down (x, y) = (x, y + 1)

left :: Point -> Point
left (x, y) = (x - 1, y)

right :: Point -> Point
right (x, y) = (x + 1, y)

rock :: Char
rock = '#'

air :: Char
air = '.'

sandSource :: Char
sandSource = '+'

sand :: Char
sand = 'o'

range' :: (Point, Point) -> [Point]
range' (pointA@(xa, ya), pointB@(xb, yb))
  | xa < xb && ya == yb = range (pointA, pointB)
  | xa > xb && ya == yb = range (pointB, pointA)
  | xa == xb && ya < yb = range (pointA, pointB)
  | otherwise = range (pointB, pointA)

readInputFile :: IO String
readInputFile = readFile "./test.txt"

pointParser :: Parsec.Parsec String () Point
pointParser = do
  [x, y] <- Parsec.sepBy (Parsec.many1 Parsec.digit) (Parsec.char ',')
  return (parseInt x, parseInt y)

pathParser :: Parsec.Parsec String () [[Point]]
pathParser = do
  let path = Parsec.sepBy pointParser (Parsec.string " -> ")
  Parsec.sepBy path Parsec.newline

parsePaths :: String -> [[Point]]
parsePaths = fromRight [] . Parsec.parse pathParser "Path Parser"

buildCaveSystem :: [Point] -> Grid Char
buildCaveSystem = foldl insertPointsToGrid (Map.singleton (500, 0) sandSource)
  where
    insertPointsToGrid grid point = Map.insert point rock grid

generatePointsFromPath :: [Point] -> [Point] -> [Point]
generatePointsFromPath path (pointA:pathDescriptor) =
  if null pathDescriptor then
    path
  else
    generatePointsFromPath newPath pathDescriptor
  where
    pointB = head pathDescriptor
    newPath = path ++ range' (pointA, pointB)

generatePointsFromPath path [] = path

gridMaxY :: Grid Char -> Int
gridMaxY = maximum . map snd . Map.keys

main :: IO ()
main = do
  inp <- readInputFile
  let pathDescriptors = parsePaths inp
  let rockyPaths = concatMap (generatePointsFromPath []) pathDescriptors
  let caveSystem = buildCaveSystem rockyPaths
  print $ part1 caveSystem
  print $ part2 caveSystem

bySandOrRock :: Char -> Bool
bySandOrRock c = c == rock || c == sand

isBlockedDown :: Point -> Grid Char -> Bool
isBlockedDown location caves = bySandOrRock $ fromMaybe air (down location `Map.lookup` caves)

isBlockedDownLeft :: Point -> Grid Char -> Bool
isBlockedDownLeft location caves = bySandOrRock $ fromMaybe air ((down . left) location `Map.lookup` caves)

isBlockedDownRight ::  Point -> Grid Char -> Bool
isBlockedDownRight location caves = bySandOrRock $ fromMaybe air ((down . right) location `Map.lookup` caves)

getSlidingDirection :: Point -> Grid Char -> Point
getSlidingDirection location caves
  | not (isBlockedDown location caves) = down location
  | not (isBlockedDownLeft location caves) = (down . left) location
  | not (isBlockedDownRight location caves) = (down . right) location
  | otherwise = location

slideSand :: Grid Char -> Int -> Point -> Int
slideSand caves count location@(_, y)
  | y == gridMaxY caves = count
  | isStuck = slideSand (Map.insert location sand caves) (count + 1) (500, 0)
  | otherwise = slideSand caves count (getSlidingDirection location caves)
  where
    isStuck = isBlockedDown location caves && isBlockedDownLeft location caves && isBlockedDownRight location caves

part1 :: Grid Char -> Int
part1 caveSystem = slideSand caveSystem 0 (500, 0)

slideSandOnInfiniteFloor :: Grid Char -> Int -> Int
slideSandOnInfiniteFloor caveSystem maxY = go caveSystem 1 (500, 0)
  where
    go caves count location
      | getSlidingDirection location caves == (500, 0) = count
      | isStuck = go (Map.insert location sand caves) (count + 1) (500, 0)
      | otherwise = go caves count (getSlidingDirection location caves)
      where
        isFloor loc = snd loc == maxY
        isStuck = isBlockedDown location caves && isBlockedDownLeft location caves && isBlockedDownRight location caves || isFloor (down location)

part2 :: Grid Char -> Int
part2 caveSystem = slideSandOnInfiniteFloor caveSystem (gridMaxY caveSystem + 2)
