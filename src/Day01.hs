module Day01 where
import Data.List (foldl')

-- Find how many depth measurements are greater than the one before.
day1a :: String -> Int
day1a = foldl' incrementRising 0 . windowsN 1 . map read . lines

-- Find how many depth measurements are greater than the one before, grouping
-- measurements in threes.
day1b :: String -> Int
day1b = foldl' incrementRising 0 . windowsN 3 . map read . lines

-- Increment the accumulator if the values increase, otherwise leave unchanged.
incrementRising :: Int -> (Int, Int) -> Int
incrementRising acc (first, second) =
  if first < second
    then acc + 1
    else acc

-- To compare windows of length N, you actually only need to compare the first
-- element of the first window with the last element of the second window.
-- Therefore, pair up all elements with the element N steps after.
windowsN :: Int -> [a] -> [(a, a)]
windowsN n xs = zip xs $ drop n xs