module Day12 where

import Data.Char (isLower)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import qualified Data.Set as S

type Cave = String

type CaveMap = M.Map String (S.Set String)

type Path = [String]

day12a :: String -> Int
day12a input = length $ dfs caveMap S.empty "start"
  where
    caveMap = parse input

day12b :: String -> Int
day12b input = length $ dfs2 caveMap S.empty Nothing "start"
  where
    caveMap = parse input

parse :: String -> CaveMap
parse input =
  M.fromListWith S.union . concat $ do
    (from:to:_) <- splitOn "-" <$> lines input
    return [(from, S.singleton to), (to, S.singleton from)]

isSmall :: Cave -> Bool
isSmall = all isLower

dfs :: CaveMap -> S.Set Cave -> Cave -> [Path]
dfs m seen "end" = [["end"]]
dfs m seen cave = map (cave :) $ concatMap (dfs m newSeen) neighbours
  where
    neighbours = filter (`S.notMember` seen) . S.toList $ m M.! cave
    newSeen =
      if isSmall cave
        then S.insert cave seen
        else seen

dfs2 :: CaveMap -> S.Set Cave -> Maybe Cave -> Cave -> [Path]
dfs2 _ seen _ "end" = [["end"]]
dfs2 m seen twice cave = map (cave :) $ concatMap (dfs2 m newSeen newTwice) neighbours
  where
    neighbours = filter filterF . S.toList $ m M.! cave
    newSeen =
      if isSmall cave
        then S.insert cave seen
        else seen
    newTwice =
      case twice of
        Just _ -> twice
        Nothing ->
          if isSmall cave && cave `S.member` seen
            then Just cave
            else Nothing
--  Can go into a small cave if we've never gone into it before, or if we've
--  never gone into any small caves twice. We can never re-enter start!
    filterF "start" = False
    filterF n = S.notMember n newSeen || isNothing newTwice
