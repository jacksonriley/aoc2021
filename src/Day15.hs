module Day15 where

import Data.List (foldl', sortOn)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace
import Data.Maybe (mapMaybe)

type Position = (Int, Int)

type CostMap = M.Map Position Int

-- Poor man's queue
type Queue = S.Set Position

type Distances = M.Map Position Int

-- day15a :: String -> Int
day15a = dijkstra . parse

day15b :: String -> Int
day15b = dijkstra . constructLargerMap . parse

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

manhattan :: Position -> Position -> Int
manhattan (x, y) (x', y') = abs (x-x') + abs (y-y')

heuristic :: Position -> Position -> Int
heuristic a b = x where
  x = manhattan a b

popMin :: Queue -> Distances -> Position -> ((Position, Int), Queue)
popMin queue distances end = (minimumPos, queue')
  where
    filtered = mapMaybe (\pos -> M.lookup pos distances >>= (\d -> Just $ (pos, d + heuristic pos end))) $ S.toList queue
    (pos, d) = head $ sortOn snd filtered
    minimumPos = (pos, d - heuristic pos end)
    queue' = S.delete (fst minimumPos) queue

wrap :: Int -> Int
wrap x = 1 + (x - 1) `mod` 9

constructLargerMap :: CostMap -> CostMap
constructLargerMap smallMap = bigBoy where
  nX =  (+1) . maximum . map fst $ M.keys smallMap
  nY =  (+1) . maximum . map snd $ M.keys smallMap
  tiles = map (\t -> (t, manhattan (0, 0) t)) [(x, y) | x <- [0..4], y <- [0..4]]
  bigBoy = foldl' makeTile M.empty tiles
  makeTile acc ((dx, dy), dv) = M.union acc $ M.fromList $ map (\((x, y), v) -> ((x + nX * dx, y + nY * dy), wrap (v + dv))) $ M.toList smallMap


dijkstra :: CostMap -> Int
dijkstra costMap = endDistances M.! end
  where
    end = getGoal costMap
    initialQueue = S.singleton (0, 0)
    definiteDistances = M.singleton (0, 0) 0
    endDistances = go initialQueue definiteDistances S.empty
    go :: Queue -> Distances -> Queue -> Distances
    go queue distances done =
      let ((pos, d), queue') = popMin queue distances end
          ns = filter (`S.notMember` done) $ neighbours pos costMap
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
          queue'' = foldl' (\acc x -> S.insert x acc) queue' ns
          done' = S.insert pos done
       in if pos == end
            then distances'
            else go queue'' distances' done'
