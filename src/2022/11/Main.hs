{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Data.List (sortOn, stripPrefix)
import Data.List.Split
import Utils (parseInt)
import qualified Data.Map as Map
import qualified Text.Parsec as Parsec
import Data.Either (rights)
import Data.Ord (Down(Down))

data Monkey = Monkey
  { _id :: Int
  , items :: [Int]
  , operation :: Int -> Int
  , test :: Int -> Int
  , inspectTimes :: Int
  , divisor :: Int
  }


instance Show Monkey where
  show monkey = "Monkey { _id = " ++ show (_id monkey) ++ ", items = " ++ show (items monkey) ++ ", inespected = " ++ show (inspectTimes monkey) ++ " }"

readInputFile :: IO String
readInputFile = readFile "./input.txt"

operationToFunction :: String -> (Int -> Int)
operationToFunction "* old" = flip (^) (2 :: Int)
operationToFunction "+ old" = (*) 2
operationToFunction (stripPrefix "* " -> Just multiplier) = (*) $ parseInt multiplier
operationToFunction (stripPrefix "+ " -> Just addition) = (+) $ parseInt addition
operationToFunction _ = undefined

testToFunction :: String -> String -> String -> (Int -> Int)
testToFunction divisibleByStr ifTrueStr ifFalseStr = fn
  where
    divisor = parseInt divisibleByStr
    ifTrue = parseInt ifTrueStr
    ifFalse = parseInt ifFalseStr
    fn n = if n `rem` divisor == 0 then ifTrue else ifFalse

monkeyParser :: Parsec.Parsec String () Monkey
monkeyParser = do
  _ <- Parsec.string "Monkey "
  monkeyId <- Parsec.many1 Parsec.digit
  _ <- Parsec.char ':'
  _ <- Parsec.endOfLine
  _ <- Parsec.manyTill Parsec.anyChar (Parsec.try (Parsec.string "items: "))
  monkeyItems <- Parsec.many1 Parsec.digit `Parsec.sepBy` Parsec.string ", "
  _ <- Parsec.endOfLine
  _ <- Parsec.manyTill Parsec.anyChar (Parsec.try (Parsec.string "new = old "))
  op <- Parsec.manyTill Parsec.anyChar (Parsec.try Parsec.endOfLine)
  _ <- Parsec.manyTill Parsec.anyChar (Parsec.try (Parsec.string "divisible by "))
  divisibleBy <- Parsec.many1 Parsec.digit
  _ <- Parsec.endOfLine
  _ <- Parsec.manyTill Parsec.anyChar (Parsec.try (Parsec.string "If true: throw to monkey "))
  ifTrue <- Parsec.many1 Parsec.digit
  _ <- Parsec.endOfLine
  _ <- Parsec.manyTill Parsec.anyChar (Parsec.try (Parsec.string "If false: throw to monkey "))
  ifFalse <- Parsec.many1 Parsec.digit
  return (
    Monkey
      { _id = parseInt monkeyId
      , items = map parseInt monkeyItems
      , operation = operationToFunction op
      , test = testToFunction divisibleBy ifTrue ifFalse
      , divisor = parseInt divisibleBy
      , inspectTimes = 0
      }
    )

createMonkeyMap :: [Monkey] -> Map.Map Int Monkey
createMonkeyMap = foldl insertMonkey Map.empty
  where
    insertMonkey monkeyMap monkey = Map.insert (_id monkey) monkey monkeyMap

throwItem :: Monkey -> Map.Map Int Monkey -> Int -> Map.Map Int Monkey
throwItem monkey monkeys item = Map.insertWith addItemsToMonkey (test monkey item) monkey monkeys
  where
    addItemsToMonkey _ oldMonkey = oldMonkey { items = items oldMonkey ++ [item] }

executeMonkeyOp :: (Int -> Int) -> Map.Map Int Monkey -> Monkey -> Map.Map Int Monkey
executeMonkeyOp worryReducer monkeyMap monkey = res
  where
    monkeyItems = map (operation monkey) $ items monkey
    loweredWorry = map worryReducer monkeyItems
    updatedMonkeyMap = foldl (throwItem monkey) monkeyMap loweredWorry
    res = Map.insert (_id monkey) monkey { items = [], inspectTimes = inspectTimes monkey + length monkeyItems } updatedMonkeyMap

runRound :: (Int -> Int) -> Map.Map Int Monkey -> Int -> Int -> Map.Map Int Monkey
runRound worryReducer monkeyMap currMonkey numOfRounds
  | numOfRounds == 0 = monkeyMap
  | currMonkey > Map.size monkeyMap - 1 = runRound worryReducer monkeyMap 0 $ numOfRounds - 1
  | otherwise = runRound worryReducer (executeMonkeyOp worryReducer monkeyMap (monkeyMap Map.! currMonkey)) (currMonkey + 1) numOfRounds

main :: IO ()
main = do
  inp <- readInputFile
  let monkeys = rights $ map (Parsec.parse monkeyParser "") $ splitOn "\n\n" inp
  let monkeyMap = createMonkeyMap monkeys
  print $ part1 monkeyMap
  print $ part2 monkeyMap

part1 :: Map.Map Int Monkey -> Int
part1 monkeyMap =
  product $ take 2 $ sortOn Down $ Map.elems $ Map.map inspectTimes $ runRound (`div` 3) monkeyMap 0 20

part2 :: Map.Map Int Monkey -> Int
part2 monkeyMap = 
  let
    worryReducer = (product $ Map.elems $ Map.map divisor monkeyMap)
  in 
    product $ take 2 $ sortOn Down $ Map.elems $ Map.map inspectTimes $ runRound (`mod` worryReducer) monkeyMap 0 10000

