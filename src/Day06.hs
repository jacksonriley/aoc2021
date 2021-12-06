module Day06 where

import Data.List (partition)
import Data.List.Split (splitOn)

day6a :: String -> Int
day6a input = sum $ iterate simulateDay (parse input) !! 80

day6b :: String -> Int
day6b input = sum $ iterate simulateDay (parse input) !! 256

-- Returns a list of integers, where each index represents a stage of fish
-- lifecycle, and each element represents the number of fish who are at that
-- stage.
parse :: String -> [Int]
parse input = go 0 [] $ map read $ splitOn "," input
  where
    go _ done [] = done ++ replicate (9 - length done) 0
    go n done rawNums =
      let (newlyDone, yetToDo) = partition (== n) rawNums
       in go (n + 1) (done ++ [length newlyDone]) yetToDo

-- Simulates a day:
--  - The number of fish at index 0 determine how many fish should be added to
--    those at index 7 (day 6) and at the end of the list (day 8)
--  - The rest of the fish each decrease their day by 1, which is naturally
--    done by taking the head off of the initial list
simulateDay :: [Int] -> [Int]
simulateDay (spawners:rest) = zeroToFive ++ six ++ seven ++ eight
  where
    zeroToFive = take 6 rest
    six = [spawners + rest !! 6]
    seven = [rest !! 7]
    eight = [spawners]
simulateDay [] = error "Should never be called with an empty list"
