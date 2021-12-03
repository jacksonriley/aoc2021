module Day03 where

import Data.List (foldl', transpose)

day3a :: String -> Int
day3a input = product $ map binToDec [gamma, epsilon]
  where
    columns = transpose $ parse input
    gamma = map (findCommon (>)) columns
    epsilon = map (1 -) gamma

day3b :: String -> Int
day3b input = product $ map binToDec [o2, co2]
  where
    nums = parse input
    o2 = findRating (>=) nums
    co2 = findRating (<) nums

-- Parses e.g. "101\n011" into [[1, 0, 1], [0, 1, 1]].
parse :: String -> [[Int]]
parse = map (map (read . pure)) . lines

-- Convert binary to decimal - e.g. [1, 0, 1] to 5.
binToDec :: [Int] -> Int
binToDec = foldl' f 0
  where
    f acc x = 2 * acc + x

-- Find the one number that satisfies all of the bit criteria.
findRating :: (Int -> Int -> Bool) -> [[Int]] -> [Int]
findRating comp = go 0
  where
    go n input =
      let filterBit = findCommon comp $ transpose input !! n
       in case filter (\xs -> xs !! n == filterBit) input of
            [answer] -> answer
            remaining -> go (n + 1) remaining

-- Find most or least common bit depending on the comparison operator passed
-- in.
findCommon :: (Int -> Int -> Bool) -> [Int] -> Int
findCommon comp xs =
  let ones = length $ filter (== 1) xs
      zeros = length $ filter (== 0) xs
   in if ones `comp` zeros
        then 1
        else 0
