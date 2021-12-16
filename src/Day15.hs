module Day15 where

import Data.List (foldl', insertBy, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S
import Debug.Trace

type Position = (Int, Int)

type CostMap = M.Map Position Int

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
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

wrap :: Int -> Int
wrap x = 1 + (x - 1) `mod` 9

constructLargerMap :: CostMap -> CostMap
constructLargerMap smallMap = bigBoy
  where
    nX = (+ 1) . maximum . map fst $ M.keys smallMap
    nY = (+ 1) . maximum . map snd $ M.keys smallMap
    tiles = map (\t -> (t, manhattan (0, 0) t)) [(x, y) | x <- [0 .. 4], y <- [0 .. 4]]
    bigBoy = foldl' makeTile M.empty tiles
    makeTile acc ((dx, dy), dv) =
      M.union acc $
      M.fromList $
      map (\((x, y), v) -> ((x + nX * dx, y + nY * dy), wrap (v + dv))) $ M.toList smallMap

dijkstra :: CostMap -> Int
dijkstra costMap = endDistances M.! end
  where
    end = getGoal costMap
    initialQueue = [((0, 0), 0)]
    definiteDistances = M.singleton (0, 0) 0
    endDistances = go initialQueue definiteDistances
    go :: [(Position, Int)] -> Distances -> Distances
    go queue distances =
      let ((pos, d):queue') = queue
          -- Never have to reconsider neighbours as the cost into a given vertex is the same from any neighbour, and if a neighbour is in `distances`, we've already processed it from a closer neighbour
          ns = filter (`M.notMember` distances) $ neighbours pos costMap
          (distances', queue'') = foldl' foldNeighbours (distances, queue') ns
          foldNeighbours ::
               (Distances, [(Position, Int)]) -> Position -> (Distances, [(Position, Int)])
          foldNeighbours (foldDists, foldQ) n =
            let nEntry = costMap M.! n
             in (M.insert n (d + nEntry) foldDists, insertBy (comparing snd) (n, d + nEntry) foldQ)
       in if pos == end
            then distances'
            else go queue'' distances'
