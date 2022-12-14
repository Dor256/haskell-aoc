module Utils (module Utils) where
import Data.List.Split (splitOn)
import qualified Data.Map as Map

type Point = (Int, Int)

type Grid a = Map.Map Point a

up :: Grid a -> Maybe (Point, a) -> Maybe (Point, a)
up grid maybeItem = case maybeItem of
    Just ((x, y), _) ->
      let
        maybePoint = (x, y - 1)
      in
      case Map.lookup maybePoint grid of
        Just value -> Just (maybePoint, value)
        Nothing -> Nothing
    Nothing -> Nothing


down :: Grid a -> Maybe (Point, a) -> Maybe (Point, a)
down grid maybeItem = case maybeItem of
    Just ((x, y), _) ->
      let
        maybePoint = (x, y + 1)
      in
      case Map.lookup maybePoint grid of
        Just value -> Just (maybePoint, value)
        Nothing -> Nothing
    Nothing -> Nothing


left :: Grid a -> Maybe (Point, a) -> Maybe (Point, a)
left grid maybeItem = case maybeItem of
    Just ((x, y), _) ->
      let
        maybePoint = (x - 1, y)
      in
      case Map.lookup maybePoint grid of
        Just value -> Just (maybePoint, value)
        Nothing -> Nothing
    Nothing -> Nothing


right :: Grid a -> Maybe (Point, a) -> Maybe (Point, a)
right grid maybeItem = case maybeItem of
    Just ((x, y), _) ->
      let
        maybePoint = (x + 1, y)
      in
      case Map.lookup maybePoint grid of
        Just value -> Just (maybePoint, value)
        Nothing -> Nothing
    Nothing -> Nothing

parseInt :: String -> Int
parseInt x = read x :: Int

next :: (Enum a, Bounded a, Eq a) => a -> a
next a = if a == maxBound then minBound else succ a

prev :: (Enum a, Bounded a, Eq a) => a -> a
prev a = if a == minBound then maxBound else pred a

toEnum' :: Enum a => Int -> a
toEnum' x = toEnum (x - 1)

fromEnum' :: Enum a => a -> Int
fromEnum' x = fromEnum x + 1

halve :: [a] -> ([a], [a])
halve xs =
  splitAt half xs
  where
    half = length xs `div` 2

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

mapTuple3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple3 f (a1, a2, a3) = (f a1, f a2, f a3)

readTuple3 :: String -> String -> (String, String, String)
readTuple3 delim s = (a, b, c)
  where
    [a, b, c] = splitOn delim s

fst3 :: (a, b, c) -> a
fst3 (a, _, _)  = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c
