module Day09 where

import Data.List (foldl', sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Position = (Int, Int)

day9a :: String -> Int
day9a input = sum $ map ((+) 1 . snd) lowPoints
  where
    tileMap = parse input
    lowPoints = findLowPoints tileMap

day9b :: String -> Int
day9b = product . take 3 . reverse . sort . map S.size . findBasins . parse

parse :: String -> M.Map Position Int
parse input =
  M.fromList $ do
    (y, l) <- zip [0 ..] $ lines input
    (x, val) <- zip [0 ..] $ map (read . pure) l
    return ((x, y), val)

-- Find the adjacent positions to a given position
adjacent :: Position -> [Position]
adjacent (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- Find all of the low points
findLowPoints :: M.Map Position Int -> [(Position, Int)]
findLowPoints tileMap = M.foldlWithKey' go [] tileMap
  where
    go acc pos val =
      if all (higher val) $ adjacent pos
        then (pos, val) : acc
        else acc
    higher val pos' =
      case M.lookup pos' tileMap of
        Just val' -> val' > val
        Nothing -> True

-- Find all of the basins by doing a flood fill from each low point.
findBasins :: M.Map Position Int -> [S.Set Position]
findBasins tileMap =
  let lowPoints = map fst $ findLowPoints tileMap
      basins = foldl' (\acc low -> floodFill tileMap [low] S.empty : acc) [] lowPoints
   in basins

-- Flood fill from a given set of positions until a wall of 9s or the edge.
floodFill :: M.Map Position Int -> [Position] -> S.Set Position -> S.Set Position
floodFill tileMap (x:xs) seen = floodFill tileMap toExplore newSeen
  where
    newSeen = S.insert x seen
    toExplore = xs ++ filter (\pos -> notSeen pos && notNine pos) (adjacent x)
    notSeen pos = not $ S.member pos seen
    notNine pos =
      case M.lookup pos tileMap of
        Just val -> val /= 9
        Nothing -> False
floodFill _ [] seen = seen
