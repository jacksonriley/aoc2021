module Day07 where

import Data.List (foldl', sort)
import Data.List.Split (splitOn)

-- The most efficient position for the crabs to get to is the median, as we
-- seek to minimise the total fuel `t` as a function of position `x`:
-- t(x) = sum_i(|x - x_i|)
-- dt/dx = sum_i(signum(x - x_i)) = 0 for minimum t
-- This is zero when the number of elements greater than x is equal to the
-- number of elements less than x, the definition of the median.
-- (There may be multiple positions which satisfy this condition, but they will
-- all have the same total fuel cost!)
day7a :: String -> Int
day7a input = totalLinearFuel median sorted
  where
    median = sorted !! (length sorted `div` 2)
    sorted = sort . parse $ input

-- The most efficient position for the crabs to get to is close to the mean, as
-- we seek to minimise the total fuel `t` as a function of position `x`:
-- Each individual fuel cost between x_i and x is now 1 + 2 + ... + |x - x_i| = 0.5*((x - x_i)^2 + |x - x_i|)
-- t(x) = 0.5 * sum_i[(x - x_i)^2 + |x - x_i|]
-- dt/dx = 0.5 * sum_i[2x - 2x_i + signum(x - x_i)] = 0 for minimum t
-- sum_i(x) = sum_i(x_i) - 0.5 * sum_i(signum(x - x_i)) but sum_i(x) is just n * x.
-- x = (1/n) * sum_i(x_i) - (1/2n) * sum_i(signum(x - x_i)) but (1/n) * sum_i(x_i) is just the definition of the mean.
-- x = mean - (1/2n) * sum_i(signum(x - x_i))
-- The max/min of sum_i(signum(x - x_i)) is ±n.
-- Therefore, x = mean ± 1/2
-- Restricting x to be an integer means that x is min(floor(mean), ceil(mean)).
--
-- NB - we don't have to check e.g. floor(mean) - 1 or ceil(mean) + 1 because
-- the fuel function is purely quadratic between each pair of crabs. This means
-- that if we have e.g. mean < n < minimum < n + 1, because crabs are only ever
-- at integers, the fuel function is guaranteed to be symmetric about the
-- minimum between n and n + 1. The minimum being within 0.5 of the mean means
-- that minimum < n + 1/2, resulting in f(n) < f(n + 1) (so we don't have to
-- check f(n + 1)).
-- Similar reasoning goes for n < minimum < n + 1 < mean, so we only ever have
-- to check floor(mean) and ceil(mean).
day7b :: String -> Int
day7b input = min (totalTriangularFuel (ceiling mean) nums) (totalTriangularFuel (floor mean) nums)
  where
    mean = fromIntegral (sum nums) / fromIntegral (length nums)
    nums = parse input

parse :: String -> [Int]
parse = map read . splitOn ","

-- Find the total fuel required for all crabs to get to position n, given a
-- cost function.
totalFuel :: (Int -> Int) -> Int -> [Int] -> Int
totalFuel costF n = foldl' (\acc x -> acc + costF (abs (x - n))) 0

-- Find the total fuel required for all crabs to get to position n when the
-- cost is linear.
totalLinearFuel :: Int -> [Int] -> Int
totalLinearFuel = totalFuel id

-- Find the total fuel required for all crabs to get to position n when the
-- cost is 1 + 2 + ... + d = 0.5 * (d^2 + d).
totalTriangularFuel :: Int -> [Int] -> Int
totalTriangularFuel = totalFuel tri
  where
    tri d = (d ^ 2 + d) `div` 2
