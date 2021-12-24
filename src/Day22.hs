module Day22 where

import Data.List (find, foldl', group, sort)
import Data.List.Split (splitOn)
import qualified Data.Set as S

type CuboidPos = ((Int, Int), (Int, Int), (Int, Int))

data Switch
  = On
  | Off
  deriving (Show)

data Step =
  Step Switch CuboidPos
  deriving (Show)

day22a :: String -> Int
day22a = sum . map volume . foldl' foldOnCuboids [] . filter filterA . parse

day22b :: String -> Int
day22b = sum . map volume . foldl' foldOnCuboids [] . parse

parse :: String -> [Step]
parse = map parseLine . lines

parseLine :: String -> Step
parseLine line = Step switch ((xLow, xHigh), (yLow, yHigh), (zLow, zHigh))
  where
    [s, positions] = splitOn " " line
    switch
      | s == "on" = On
      | s == "off" = Off
      | otherwise = error $ "Unexpected switch: " ++ s
    [[xLow, xHigh], [yLow, yHigh], [zLow, zHigh]] =
      map (map read . splitOn ".." . drop 2) $ splitOn "," positions

filterA :: Step -> Bool
filterA (Step _ ((xLow, xHigh), (yLow, yHigh), (zLow, zHigh))) =
  all (`elem` [-50 .. 50]) [xLow, xHigh, yLow, yHigh, zLow, zHigh]

-- Plan - track cuboids which are totally on.
-- If an on cuboid intersects with another on cuboid, create new on cuboids to
-- represent their intersection.
-- If an off cuboid intersects with an on cuboid, create new on cuboids from
-- what's left of the on cuboid.
-- These are actually equivalent, but either the new cuboid gets added in its
-- entirety to the list of on cuboids or not.
-- Intersection if for all coordinates, at least one boundary lies within the
-- range of the boundaries of the other cuboid.
intersects :: CuboidPos -> CuboidPos -> Bool
intersects (xs, ys, zs) (xs', ys', zs') = within xs xs' && within ys ys' && within zs zs'

within :: (Int, Int) -> (Int, Int) -> Bool
within a b = go a b || go b a
  where
    go (x1, x2) (x1', x2') = within1 (x1, x2) x1' || within1 (x1, x2) x2'

within1 :: (Int, Int) -> Int -> Bool
within1 (x1, x2) x1' = x1 <= x1' && x1' <= x2

-- Split the first cuboid into constituent cuboids that are in the first cuboid
-- and not in the second cuboid. Do this for each boundary plane by splitting
-- off any bit of the cuboid which goes past the boundary plane, and recursing
-- on what's left. This means a maximum of six splits.
split :: CuboidPos -> CuboidPos -> [CuboidPos]
split c1 c2 = go c1 c2 []
  where
    go :: CuboidPos -> CuboidPos -> [CuboidPos] -> [CuboidPos]
    go c@((x1, x2), (y1, y2), (z1, z2)) s@((x1', x2'), (y1', y2'), (z1', z2')) splits
      | not $ intersects c s = pure c
      | (x1 < x1' && x1' <= x2) && within (y1, y2) (y1', y2') && within (z1, z2) (z1', z2') =
        go ((x1', x2), (y1, y2), (z1, z2)) s (((x1, x1' - 1), (y1, y2), (z1, z2)) : splits)
      | (x1 <= x2' && x2' < x2) && within (y1, y2) (y1', y2') && within (z1, z2) (z1', z2') =
        go ((x1, x2'), (y1, y2), (z1, z2)) s (((x2' + 1, x2), (y1, y2), (z1, z2)) : splits)
      | within (x1, x2) (x1', x2') && (y1 < y1' && y1' <= y2) && within (z1, z2) (z1', z2') =
        go ((x1, x2), (y1', y2), (z1, z2)) s (((x1, x2), (y1, y1' - 1), (z1, z2)) : splits)
      | within (x1, x2) (x1', x2') && (y1 <= y2' && y2' < y2) && within (z1, z2) (z1', z2') =
        go ((x1, x2), (y1, y2'), (z1, z2)) s (((x1, x2), (y2' + 1, y2), (z1, z2)) : splits)
      | within (x1, x2) (x1', x2') && within (y1, y2) (y1', y2') && (z1 < z1' && z1' <= z2) =
        go ((x1, x2), (y1, y2), (z1', z2)) s (((x1, x2), (y1, y2), (z1, z1' - 1)) : splits)
      | within (x1, x2) (x1', x2') && within (y1, y2) (y1', y2') && (z1 <= z2' && z2' < z2) =
        go ((x1, x2), (y1, y2), (z1, z2')) s (((x1, x2), (y1, y2), (z2' + 1, z2)) : splits)
      | otherwise = splits

foldOnCuboids :: [CuboidPos] -> Step -> [CuboidPos]
foldOnCuboids onCuboids (Step switch newCuboid) = x
  where
    x = foldl' (\acc c -> split c newCuboid ++ acc) base onCuboids
    base =
      case switch of
        On -> [newCuboid]
        Off -> []

volume :: CuboidPos -> Int
volume ((x1, x2), (y1, y2), (z1, z2)) = (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)
