module Day05 where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type Position = (Int, Int)

day5a :: String -> Int
day5a = getNumCrossingPoints . filter horizontalOrVertical . parse
  where
    horizontalOrVertical ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

day5b :: String -> Int
day5b = getNumCrossingPoints . parse

-- Given pairs of endpoints, get the number of positions at which two or more
-- segments cross.
getNumCrossingPoints :: [(Position, Position)] -> Int
getNumCrossingPoints =
  length .
  M.filter (> 1) .
  M.fromListWith (+) . flip zip (repeat 1) . concatMap getPoints

-- Get all of the points between two positions (assuming that they lie either
-- horizontally, vertically, or on a diagonal with gradient 1)
getPoints :: (Position, Position) -> [Position]
getPoints ((x1, y1), (x2, y2)) = zip xs ys
  where
    x1' =
      if x1 < x2
        then x1 + 1
        else x1 - 1
    y1' =
      if y1 < y2
        then y1 + 1
        else y1 - 1
    -- This [x1, x1' .. x2] business is required to let Haskell know whether
    -- the numbers are increasing or decreasing, as unhelpfully e.g. [3 .. 1] is []
    xs =
      if x1 == x2
        then repeat x1
        else [x1,x1' .. x2]
    ys =
      if y1 == y2
        then repeat y1
        else [y1,y1' .. y2]

parse :: String -> [(Position, Position)]
parse input = do
  line <- lines input
  let (first:second:_) = splitOn " -> " line
  let (x1:y1:_) = splitOn "," first
  let (x2:y2:_) = splitOn "," second
  return ((read x1, read y1), (read x2, read y2))
