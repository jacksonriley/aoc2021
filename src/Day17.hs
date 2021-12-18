module Day17 where

import Data.List (find)
import Data.List.Split (splitOn)

data Position =
  Position Int Int
  deriving (Show)

data Velocity =
  Velocity Int Int
  deriving (Show)

-- The probe always passes through y=0 again because the trajectory is
-- symetrical about the highest point. Therefore, the maximum y velocity worth
-- checking is that of abs minY.
-- The highest x velocity worth checking is technically maxX but
-- realistically... maxX/2?
day17a :: String -> Int
day17a input = highPoint maxVy
  where
    bounds@(minX, maxX, minY, maxY) = parse input
    vys = [(abs minY),(abs minY - 1) .. 0]
    vxs = [0 .. maxX]
    vs = [Velocity vx vy | vx <- vxs, vy <- vys]
    Just (Velocity maxVx maxVy) = find (velocityWorks bounds) vs

day17b :: String -> Int
day17b input = length $ filter (velocityWorks bounds) vs
  where
    bounds@(minX, maxX, minY, maxY) = parse input
    vys = [(abs minY),(abs minY - 1) .. minY]
    vxs = [0 .. maxX]
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
  | otherwise = trajectoryEnters bounds ps
trajectoryEnters _ [] = False

velocityWorks :: (Int, Int, Int, Int) -> Velocity -> Bool
velocityWorks bounds = trajectoryEnters bounds . points (Position 0 0)

highPoint :: Int -> Int
highPoint 0 = 0
highPoint v = v + highPoint (v - 1)
