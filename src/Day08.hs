module Day08 where

import Data.List.Split (splitOn)
import qualified Data.Set as S

day8a :: String -> Int
day8a =
  length . filter (\x -> length x `elem` [2, 3, 4, 7]) . concatMap snd . parse

day8b :: String -> Int
day8b = undefined

parse :: String -> [([S.Set Char], [String])]
parse = map parseLine . lines

parseLine :: String -> ([S.Set Char], [String])
parseLine l = (map S.fromList signals, digits)
  where
    (signals:digits:_) = map (splitOn " ") $ splitOn " | " l
