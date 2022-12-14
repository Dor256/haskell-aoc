module Main (main) where
import Data.List.Split (splitOn)
import Utils (parseInt, mapTuple)

type Assignment = (Int, Int)

readInputFile :: IO String
readInputFile = readFile "./test.txt"

main :: IO ()
main = do
  inp <- readInputFile
  print $ part1 (lines inp)
  print $ part2 (lines inp)

parseElfPairs :: String -> (Assignment, Assignment)
parseElfPairs input = pair
  where
    [firstElf, secondElf] = splitOn "," input
    parseAssignments = splitOn "-"
    pair = mapTuple
      (\tup -> 
        let 
          [x, xs] = parseAssignments tup
        in (parseInt x, parseInt xs)
      )
      (firstElf, secondElf)

includes :: (Assignment, Assignment) -> Bool
includes ((a, b), (c, d)) =
  a <= c && b >= d || a >= c && b <= d

part1 :: [String] -> Int
part1 inp = res
  where
    pairs = map parseElfPairs inp
    res = length $ filter includes pairs

intersects :: (Assignment, Assignment) -> Bool
intersects ((a, b), (c, d)) =
  a <= d && b >= c

part2 :: [String] -> Int
part2 inp = res
  where
    pairs = map parseElfPairs inp
    res = length $ filter intersects pairs
