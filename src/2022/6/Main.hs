module Main (main) where
import Data.List (tails, group, sort, findIndex)
import Data.Maybe (fromJust)

readInputFile :: IO String
readInputFile = readFile "./test.txt"

getSubstringsOfLength :: Int -> String -> Int
getSubstringsOfLength len = (+len) . fromJust . findIndex isUniqueSequence . slide len

isUniqueSequence :: String -> Bool
isUniqueSequence str = length (group $ sort str) == length str

slide :: Int -> String -> [String]
slide n = map (take n) . tails

main :: IO ()
main = do
  inp <- readInputFile
  print $ part1 inp
  print $ part2 inp

part1 :: String -> Int
part1 = getSubstringsOfLength 4

part2 :: String -> Int
part2 = getSubstringsOfLength 14
