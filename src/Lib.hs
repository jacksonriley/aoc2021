module Lib
  ( libmain
  ) where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import GHC.RTS.Flags (GCFlags(numa))
import Text.Printf

getDayInput :: Int -> IO String
getDayInput i = readFile filename
  where
    filename = printf "input/%0.2d" i

runDay :: Int -> (String -> String, String -> String) -> IO ()
runDay num (part1, part2) = do
  input <- getDayInput num
  putStrLn $ show num ++ "a: " ++ part1 input
  putStrLn $ show num ++ "b: " ++ part2 input
  return ()

libmain :: Maybe Int -> IO ()
libmain x =
  case x of
    Just num -> runDay' num
    Nothing -> mapM_ runDay' [1 ..]

runDay' :: Int -> IO ()
runDay' n
  | n == 1 = runDay 1 (show . day1a, show . day1b)
  | n == 2 = runDay 2 (show . day2a, show . day2b)
  | n == 3 = runDay 3 (show . day3a, show . day3b)
  | n == 4 = runDay 4 (show . day4a, show . day4b)
  | n == 5 = runDay 5 (show . day5a, show . day5b)
  | n == 6 = runDay 6 (show . day6a, show . day6b)
  | n == 7 = runDay 7 (show . day7a, show . day7b)
  | n == 8 = runDay 8 (show . day8a, show . day8b)
  | n == 9 = runDay 9 (show . day9a, show . day9b)
  | n == 10 = runDay 10 (show . day10a, show . day10b)
  | n == 11 = runDay 11 (show . day11a, show . day11b)
  | n == 12 = runDay 12 (show . day12a, show . day12b)
  | n == 13 = runDay 13 (show . day13a, day13b)
  | n == 14 = runDay 14 (show . day14a, show . day14b)
  | n == 15 = runDay 15 (show . day15a, show . day15b)
  | n == 16 = runDay 16 (show . day16a, show . day16b)
  | n == 17 = runDay 17 (show . day17a, show . day17b)
  | n == 18 = runDay 18 (show . day18a, show . day18b)
  | n == 19 = runDay 19 (show . day19a, show . day19b)
  | n == 20 = runDay 20 (show . day20a, show . day20b)
  | n == 21 = runDay 21 (show . day21a, show . day21b)
  | n == 22 = runDay 22 (show . day22a, show . day22b)
  | otherwise = error "Haven't implemented that day yet"
