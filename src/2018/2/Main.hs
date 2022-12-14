module Main (main) where
import Data.List (sort, group, intersect)

readInputFile :: IO String
readInputFile = readFile "./test.txt"

main :: IO ()
main = do
  inp <- readInputFile
  let lns = lines inp
  print $ part1 $ filter (not . null) $ map (filter (\n -> n >= 2 && n <= 3) . map length . (group . sort)) lns
  print $ part2 lns

part1 :: [[Int]] -> Int
part1 input = res
  where
    twos = length $ filter (elem 2) input
    threes = length $ filter (elem 3) input
    res = twos * threes

part2 :: [String] -> String
part2 inp = head $ [x `intersect` y | x <- inp, y <- inp, (length . filter (uncurry (/=)) $ zip x y) == 1]
