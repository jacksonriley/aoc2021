module Day02 where

import Data.List (foldl')

data Direction
  = Forward Int
  | Up Int
  | Down Int
  deriving (Show)

day2a :: String -> Int
day2a input = position * depth where
  (position, depth) = foldl' accumDir (0, 0) . map parse . lines $ input

day2b :: String -> Int
day2b input = position * depth where
  (_, position, depth) = foldl' accumDir2 (0, 0, 0) . map parse . lines $ input

parse :: String -> Direction
parse = convert . words
  where
    convert [dir, amount]
      | dir == "forward" = Forward $ read amount
      | dir == "up" = Up $ read amount
      | dir == "down" = Down $ read amount
      | otherwise = error $ "Unknown direction: " ++ dir
    convert x = error $ "Don't know how to parse " ++ show x

accumDir :: (Int, Int) -> Direction -> (Int, Int)
accumDir (x, d) dir =
  case dir of
    Forward amount -> (x + amount, d)
    Up amount -> (x, d - amount)
    Down amount -> (x, d + amount)

accumDir2 :: (Int, Int, Int) -> Direction -> (Int, Int, Int)
accumDir2 (aim, x, d) dir =
  case dir of
    Forward amount -> (aim, x + amount, d + aim * amount)
    Up amount -> (aim - amount, x, d)
    Down amount -> (aim + amount, x, d)
