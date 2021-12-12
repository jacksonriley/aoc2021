module Day11 where

import Data.List (concatMap, foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S

type Position = (Int, Int)

day11a :: String -> Int
day11a input = snd . (!! 100) $ iterate tickI (m, 0)
  where
    m = parse input

day11b :: String -> Int
day11b = tickFindSync . parse

naiveNeighbours :: Position -> [Position]
naiveNeighbours (x, y) = [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], x /= x' || y /= y']

neighbours :: Position -> M.Map Position a -> [Position]
neighbours (x, y) m = foldl' go [] $ naiveNeighbours (x, y)
  where
    go acc n =
      case M.lookup n m of
        Just _ -> n : acc
        Nothing -> acc

parse :: String -> M.Map Position Int
parse input =
  M.fromList $ do
    (y, l) <- zip [0 ..] $ lines input
    (x, val) <- zip [0 ..] $ map (read . pure) l
    return ((x, y), val)

tickI :: (M.Map Position Int, Int) -> (M.Map Position Int, Int)
tickI (m, acc) = (m', acc + newZeroed)
  where
    (m', newZeroed) = tick m

tickFindSync :: M.Map Position Int -> Int
tickFindSync m = go 1 m
  where
    go n m =
      let (m', numFlashes) = tick m
       in if numFlashes == 100
            then n
            else go (n + 1) m'

-- Return the new map and the number of octopuses that flashed
tick :: M.Map Position Int -> (M.Map Position Int, Int)
tick m =
  let incremented = M.map (+ 1) m
      flashed = flash incremented S.empty
      zeroed =
        M.map
          (\v ->
             if v > 9
               then 0
               else v)
          flashed
      numZeroes =
        M.foldl'
          (\acc v ->
             if v == 0
               then acc + 1
               else acc)
          0
          zeroed
   in (zeroed, numZeroes)

flash :: M.Map Position Int -> S.Set Position -> M.Map Position Int
flash m flashed
  -- The octopi to flash are those with values >=9 and who have not already flashed.
 =
  let toFlash =
        S.fromList .
        map fst .
        filter (\(pos, val) -> val > 9 && not (pos `S.member` flashed)) $
        M.toList m
      m' =
        foldl'
          (\acc pos ->
             foldl' (\acc' n -> M.insertWith (+) n 1 acc') acc $
             neighbours pos m)
          m $
        S.toList toFlash
   in if S.null toFlash
        then m
        else flash m' $ S.union flashed toFlash
