module Day03 where

day3a :: String -> Int
day3a = product . map binToDec . calculateGammaAndEpsilon . parse

day3b :: String -> Int
day3b = undefined

-- Parses e.g. "101\n011" into [[1,0], [0,1], [1,1]]
parse :: String -> [[Int]]
parse = transpose . map (map (read . pure)) . lines

-- Transposes a list of lists - e.g. takes [[1, 2], [3, 4]] to [[1, 3], [2, 4]]
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

-- Calculate gamma and epsilon by using a "is the sum greater than length/2"
-- definition for most common bit
calculateGammaAndEpsilon :: [[Int]] -> [[Int]]
calculateGammaAndEpsilon x = [map convertG summed, map convertD summed] where
    summed = map sum x
    boundary = length (head x) `div` 2
    convertG s = if s > boundary then 1 else 0
    convertD s = if s > boundary then 0 else 1

-- Convert binary to decimal - e.g. [1, 0, 1] to 5
binToDec :: [Int] -> Int
binToDec = fst . foldr f (0, 1) where
    f x (acc, p) = (acc + x*p, p*2)