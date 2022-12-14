{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils (parseInt)

data Claim = Claim
  { _id :: Int
  , x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  } deriving (Show)

readInputFile :: IO String
readInputFile = readFile "./input.txt"

squares :: Claim -> [(Int, Int)]
squares Claim {..} =
  [ (x + dx, y + dy)
  | dx <- [0..width - 1]
  , dy <- [0..height - 1]
  ]

parse :: String -> Claim
parse = readClaim . map parseInt . split (dropDelims . dropBlanks $ oneOf "# @,:x")
  where
    readClaim [id, x, y, width, height] = Claim id x y width height

overlap :: [Claim] -> Set.Set (Int, Int)
overlap = Map.keysSet . Map.filter (>=2) . Map.fromListWith (+) . map (, 1) . concatMap squares

main :: IO ()
main = do
  inp <- readInputFile
  let claims = map parse (lines inp)
  let overlapped = overlap claims
  print $ overlap claims
  print $ part1 overlapped
  print $ part2 overlapped claims

part1 :: Set.Set (Int, Int) -> Int
part1 = length

isOverlapping :: Set.Set (Int, Int) -> Claim -> Bool
isOverlapping overlaps = all (`Set.notMember` overlaps) . squares

part2 :: Set.Set (Int, Int) -> [Claim] -> Int
part2 overlapping = _id . head . filter (isOverlapping overlapping)
