module Day17 where

import Data.List (find)
import Data.List.Split (splitOn)

data Position =
  Position Int Int
  deriving (Show)

data Velocity =
  Velocity Int Int
  deriving (Show)

-- The probe always passes through y=0 again with v' = -v0 because the
-- trajectory is symetrical about the highest point. Therefore, the maximum y
-- velocity which gets us into bounds is (minY - 1) (such that the next Y point
-- after going through y=0 again is the bottom of the target area.)
day17a :: String -> Int
day17a input = highPoint (abs minY - 1)
  where
    (_, _, minY, _) = parse input

-- The highest x velocity worth checking is maxX
-- The lowest x velocity worth checking is the inverse of the triangle number
-- above minX.
-- The highest y velocity worth checking is (abs minY - 1) as in part a
-- The lowest y velocity worth checking is minY.
day17b :: String -> Int
day17b input = length $ filter (velocityWorks bounds) vs
  where
    bounds@(minX, maxX, minY, maxY) = parse input
    vys = [minY .. (abs minY - 1)]
    vxs = [(computeLowVxBound minX) .. maxX]
    vs = [Velocity vx vy | vx <- vxs, vy <- vys]

parse :: String -> (Int, Int, Int, Int)
parse input = (minX, maxX, minY, maxY)
  where
    [_, _, xs, ys] = splitOn " " input
    [minX, maxX] = map read . splitOn ".." . drop 2 $ init xs
    [minY, maxY] = map read . splitOn ".." $ drop 2 ys

points :: Position -> Velocity -> [Position]
points pos vel = pos : points pos' vel'
  where
    Position x y = pos
    Velocity vx vy = vel
    pos' = Position (x + vx) (y + vy)
    vel' = Velocity vx' (vy - 1)
    vx'
      | vx > 0 = vx - 1
      | vx < 0 = vx + 1
      | otherwise = 0

trajectoryEnters :: (Int, Int, Int, Int) -> [Position] -> Bool
trajectoryEnters bounds@(minX, maxX, minY, maxY) (Position x y:ps)
  | minX <= x && maxX >= x && minY <= y && maxY >= y = True
  | y < minY = False
  | x > maxX = False
  | otherwise = trajectoryEnters bounds ps
trajectoryEnters _ [] = False

velocityWorks :: (Int, Int, Int, Int) -> Velocity -> Bool
velocityWorks bounds = trajectoryEnters bounds . points (Position 0 0)

-- Max height is v0 + (v0 - 1) + ... + 1 + 0 which is just the triangular
-- number for v0.
highPoint :: Int -> Int
highPoint v = v * (v + 1) `div` 2

-- Want to find minVx such that 1 + 2 + ... + minVx < minX
-- minVx(minVx + 1) < 2*minX
computeLowVxBound :: Int -> Int
computeLowVxBound minX = (floor . sqrt $ fromIntegral (2 * minX)) - 1
