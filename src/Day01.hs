module Day01 where

day1a :: String -> Int
day1a = foldr incrementRising 0 . windowsN 1 . readNums

day1b :: String -> Int
day1b = foldr incrementRising 0 . windowsN 3 . readNums

-- Increment the accumulator if the values increase, otherwise leave unchanged.
incrementRising :: (Int, Int) -> Int -> Int
incrementRising (first, second) acc =
  if first < second
    then acc + 1
    else acc

readNums :: String -> [Int]
readNums = map read . lines

windowsN :: Int -> [a] -> [(a, a)]
windowsN n l = zip l $ drop n l