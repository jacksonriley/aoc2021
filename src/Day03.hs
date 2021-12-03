module Day03 where

day3a input = product . map binToDec $ [gamma, epsilon] where
    (gamma, epsilon) = calculateGammaAndEpsilon . parse $ input

day3b :: String -> Int
day3b = undefined

-- Parses e.g. "101\n011" into [[1,0], [0,1], [1,1]]
parse :: String -> [[Int]]
parse = transpose . map (map (read . pure)) . lines

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

calculateGammaAndEpsilon :: [[Int]] -> ([Int], [Int])
calculateGammaAndEpsilon x = (map convertG summed, map convertD summed) where
    summed = map sum x
    boundary = length (head x) `div` 2
    convertG s = if s > boundary then 1 else 0
    convertD s = if s > boundary then 0 else 1

binToDec :: [Int] -> Int
binToDec = fst . foldr f (0, 1) where
    f x (acc, p) = (acc + x*p, p*2)