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
import GHC.RTS.Flags (GCFlags(numa))
import Text.Printf

getDayInput :: Int -> IO String
getDayInput i = readFile filename
  where
    filename = printf "input/%0.2d" i

runDay :: (Show a, Show b) => Int -> (String -> a, String -> b) -> IO ()
runDay num (part1, part2) = do
  input <- getDayInput num
  putStrLn $ show num ++ "a: " ++ show (part1 input)
  putStrLn $ show num ++ "b: " ++ show (part2 input)
  return ()

libmain :: Maybe Int -> IO ()
libmain x =
  case x of
    Just num -> runDay' num
    Nothing -> mapM_ runDay' [1 ..]

runDay' :: Int -> IO ()
runDay' n
  | n == 1 = runDay 1 (day1a, day1b)
  | n == 2 = runDay 2 (day2a, day2b)
  | n == 3 = runDay 3 (day3a, day3b)
  | n == 4 = runDay 4 (day4a, day4b)
  | n == 5 = runDay 5 (day5a, day5b)
  | n == 6 = runDay 6 (day6a, day6b)
  | n == 7 = runDay 7 (day7a, day7b)
  | n == 8 = runDay 8 (day8a, day8b)
  | n == 9 = runDay 9 (day9a, day9b)
  | n == 10 = runDay 10 (day10a, day10b)
  | n == 11 = runDay 11 (day11a, day11b)
  | n == 12 = runDay 12 (day12a, day12b)
  | otherwise = error "Haven't implemented that day yet"
