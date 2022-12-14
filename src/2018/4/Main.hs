{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Data.List.Split
import Data.List
import qualified Data.Map as Map
import Data.Char (isSpace)
import qualified Text.Parsec as Parsec
import Utils (parseInt)
import Data.Either (rights)
import Control.Applicative
import Text.Read (readMaybe)

maybeParseInt :: String -> Maybe Int
maybeParseInt s = readMaybe s :: Maybe Int

readInputFile :: IO String
readInputFile = readFile "./test.txt"

eventTimeParser :: Parsec.Parsec String () (String, Int)
eventTimeParser = do
  _ <- Parsec.char '['
  _ <- Parsec.manyTill Parsec.anyChar (Parsec.try (Parsec.char '-'))
  date <- Parsec.manyTill Parsec.anyChar (Parsec.try  Parsec.space)
  _ <- Parsec.many Parsec.digit
  _ <- Parsec.char ':'
  minute <- Parsec.many Parsec.digit
  return (date, parseInt minute)

guardParser :: Parsec.Parsec String () String
guardParser = do
  _ <- Parsec.manyTill Parsec.anyChar (Parsec.try (Parsec.string "Guard"))
  _ <- Parsec.space
  _ <- Parsec.string "#"
  Parsec.many1 Parsec.digit

wakinessParser :: Parsec.Parsec String () String
wakinessParser = do
  _ <- Parsec.manyTill Parsec.anyChar (Parsec.try (Parsec.char ']'))
  _ <- Parsec.space
  Parsec.choice [Parsec.string "falls", Parsec.string "wakes"]

eventParser :: Parsec.Parsec String () String
eventParser = do
  Parsec.try wakinessParser
  <|> Parsec.try guardParser

parser :: Parsec.Parsec String () ((String, Int), String)
parser = do
  time <- eventTimeParser
  event <- eventParser
  return (time, event)

main :: IO ()
main = do
  inp <- readInputFile
  -- let schedule = map (map trim . split (dropDelims . dropBlanks $ oneOf "[]")) (lines inp)
  -- print $ lines inp
  let schedule = map (Parsec.parse eventTimeParser "") $ lines inp
  -- let events = 
  let events = rights $ map (Parsec.parse parser "") $ lines inp
  print events
  -- case Parsec.parse eventParser "" inp of
  --     Right v -> print v
  --     Left e -> print e
