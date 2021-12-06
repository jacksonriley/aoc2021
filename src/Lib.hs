module Lib
  ( libmain
  ) where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
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
  | otherwise = error "Haven't implemented that day yet"
