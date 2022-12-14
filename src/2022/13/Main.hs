module Main (main) where

import qualified Text.Parsec as Parsec
import Utils (parseInt)
import Data.Either (rights, fromRight)
import Data.List (sortBy, findIndex)
import Data.Maybe (fromJust)

data Value = Value Int | Arr [Value] deriving (Show, Eq, Ord)
type Packet = (Value, Value)

readInputFile :: IO String
readInputFile = readFile "./test.txt"

parseSingleValue :: Parsec.Parsec String () Value
parseSingleValue = do
  num <- Parsec.many1 Parsec.digit
  return (Value $ parseInt num)

parseValueList :: Parsec.Parsec String () Value
parseValueList = do
  inner <- Parsec.between (Parsec.char '[') (Parsec.char ']') $ Parsec.sepBy parseValue (Parsec.char ',')
  return (Arr inner)

parseValue :: Parsec.Parsec String () Value
parseValue = do
  parseSingleValue Parsec.<|> parseValueList

parsePacketPairs :: Parsec.Parsec String () Packet
parsePacketPairs = do
  first <- parseValue
  _ <- Parsec.newline
  second <- parseValue
  _ <- Parsec.newline
  return (first, second)

pairParser :: Parsec.Parsec String () [Packet]
pairParser = do
  Parsec.sepBy parsePacketPairs Parsec.newline

packetParser :: Parsec.Parsec String () Value
packetParser = do
  parseValue

parseAllPacketPairs :: String -> Either Parsec.ParseError [Packet]
parseAllPacketPairs = Parsec.parse pairParser "Packet Pair Parser"

parseAllPackets :: String -> Either Parsec.ParseError Value
parseAllPackets = Parsec.parse packetParser "Packet Parser"

main :: IO ()
main = do
  inp <- readInputFile
  print $ part1 inp
  print $ part2 inp

comparePackets :: Packet -> Ordering
comparePackets (left@(Arr _), right@(Value _)) = comparePackets (left, Arr [right])
comparePackets (left@(Value _), right@(Arr _)) = comparePackets (Arr [left], right)
comparePackets (Value left, Value right) = left `compare` right
comparePackets (Arr left, Arr right) = if null maybeUnequal then length left `compare` length right else head maybeUnequal
  where
    maybeUnequal = dropWhile (==EQ) $ zipWith (curry comparePackets) left right

part1 :: String -> Int
part1 input = sum $ map fst $ filter (\(_, order) -> order == LT) $ zip [1..] $ map comparePackets packetPairs
  where
    packetPairs = fromRight [] $ parseAllPacketPairs input

part2 :: String -> Int
part2 input = maybeDivider2 * maybeDivider6
  where
    sortedPackets = sortBy (curry comparePackets) $ rights $ map parseAllPackets $ filter (not . null) (lines input) ++ ["[6]", "[2]"]

    isPacketDivider divider (Arr [Value val]) = val == divider
    isPacketDivider _ _ = False

    findDivider divider = findIndex $ isPacketDivider divider

    maybeDivider2 = fromJust (findDivider 2 sortedPackets) + 1
    maybeDivider6 = fromJust (findDivider 6 sortedPackets) + 1

