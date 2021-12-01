module Lib
  ( libmain
  ) where

import Day01
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
  | otherwise = error "Haven't implemented that day yet"
