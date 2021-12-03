module Day03 where

import Data.List (foldl', group, maximumBy, sort, sortBy, transpose)

data Extremum
  = Most
  | Least

day3a :: String -> Int
day3a = product . map binToDec . calculateGammaAndEpsilon . parse

day3b :: String -> Int
day3b input =
  product $
  map
    binToDec
    [findRating Most 1 . parse $ input, findRating Least 0 . parse $ input]

-- Parses e.g. "101\n011" into [[1, 0, 1], [0, 1, 1]]
parse :: String -> [[Int]]
parse = map (map (read . pure)) . lines

-- Calculate gamma and epsilon by using a "is the sum greater than length/2"
-- definition for most common bit
calculateGammaAndEpsilon :: [[Int]] -> [[Int]]
calculateGammaAndEpsilon xs = [gamma, epsilon]
  where
    gamma = map (findCommon Most undefined) . transpose $ xs
    epsilon = map (findCommon Least undefined) . transpose $ xs

-- Convert binary to decimal - e.g. [1, 0, 1] to 5
binToDec :: [Int] -> Int
binToDec = foldl' f 0
  where
    f acc x = 2 * acc + x

-- Find the one number that satisfies all of the bit criteria
findRating :: Extremum -> Int -> [[Int]] -> [Int]
findRating extremum bias = go 0
  where
    go n input =
      let filterBit = findCommon extremum bias $ transpose input !! n
       in case filter (\xs -> xs !! n == filterBit) input of
            [answer] -> answer
            remaining -> go (n + 1) remaining

-- Find most or least common depending on the extremum passed in. Split ties
-- with the bias.
findCommon :: Extremum -> Int -> [Int] -> Int
findCommon extremum bias xs =
  let ones = length $ filter (== 1) xs
      zeros = length $ filter (== 0) xs
   in case ones `compare` zeros of
        GT ->
          case extremum of
            Most -> 1
            Least -> 0
        EQ -> bias
        LT ->
          case extremum of
            Most -> 0
            Least -> 1
