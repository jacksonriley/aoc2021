module Day15 where

import Data.List (foldl', sortOn)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace

type Position = (Int, Int)

type CostMap = M.Map Position Int

-- Poor man's queue
type Queue = S.Set Position

type Distances = M.Map Position Int

-- day15a :: String -> Int
day15a = dijkstra . parse

day15b :: String -> Int
day15b = undefined

naiveNeighbours :: Position -> [Position]
naiveNeighbours (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

neighbours :: Position -> M.Map Position a -> [Position]
neighbours (x, y) m = foldl' go [] $ naiveNeighbours (x, y)
  where
    go acc n =
      case M.lookup n m of
        Just _ -> n : acc
        Nothing -> acc

parse :: String -> CostMap
parse input =
  M.fromList $ do
    (y, l) <- zip [0 ..] $ lines input
    (x, val) <- zip [0 ..] $ map (read . pure) l
    return ((x, y), val)

getGoal :: CostMap -> Position
getGoal costMap = (maxX, maxY)
  where
    maxX = maximum . map fst $ M.keys costMap
    maxY = maximum . map snd $ M.keys costMap

popMin :: Queue -> Distances -> ((Position, Int), Queue)
popMin queue distances = (minimumPos, queue')
  where
    filtered = M.filterWithKey (\pos d -> S.member pos queue) distances
    minimumPos = head . sortOn snd $ M.toList filtered
    queue' = S.delete (fst minimumPos) queue

dijkstra :: CostMap -> Int
dijkstra costMap = endDistances M.! getGoal costMap
  where
    initialQueue = S.fromList $ M.keys costMap
    definiteDistances = M.singleton (0, 0) 0
    endDistances = snd $ go initialQueue definiteDistances
    go :: Queue -> Distances -> (Queue, Distances)
    go queue distances =
      let ((pos, d), queue') = popMin queue distances
          ns = filter (`S.member` queue) $ neighbours pos costMap
          distances' = foldl' foldNeighbours distances ns
          foldNeighbours :: Distances -> Position -> Distances
          foldNeighbours acc n =
            let nEntry = costMap M.! n
             in case M.lookup n distances of
                  Just current ->
                    if current > d + nEntry
                      then M.insert n (d + nEntry) acc
                      else acc
                  Nothing -> M.insert n (d + nEntry) acc
       in if pos == getGoal costMap
            then (queue', distances')
            else go queue' distances'
