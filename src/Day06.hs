module Day06 where

import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

day6a :: String -> Int
day6a = findFishAfter 80

day6b :: String -> Int
day6b = findFishAfter 256

-- Find the number of fish that exist after a given number of days.
findFishAfter :: Int -> String -> Int
findFishAfter nDays = sum . (!! nDays) . iterate simulateDay . parse

-- Returns a map of stage of fish lifecycle to the number of fish who are at
-- that stage.
parse :: String -> M.Map Int Int
parse =
  foldl' (\acc n -> M.insertWith (+) n 1 acc) M.empty . map read . splitOn ","

-- Simulates a day:
--  - The number of fish at day 0 determine how many fish should be added to
--    those at day 6 and day 8
--  - The rest of the fish each decrease their day by 1
simulateDay :: M.Map Int Int -> M.Map Int Int
simulateDay = M.fromListWith (+) . concatMap conv . M.toList
  where
    conv (day, num)
      | day == 0 = [(6, num), (8, num)]
      | otherwise = [(day - 1, num)]
