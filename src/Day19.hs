module Day19 where

import Data.List (foldl', maximumBy, tails, transpose)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import qualified Data.Set as S
import Debug.Trace
import Linear.V3 (V3(..))

type Position = V3 Int

-- 90 degree rotation about the x axis
rotX :: Position -> Position
rotX (V3 x y z) = V3 x (-z) y

-- 90 degree rotation about the y axis
rotY :: Position -> Position
rotY (V3 x y z) = V3 z y (-x)

-- 90 degree rotation about the z axis
rotZ :: Position -> Position
rotZ (V3 x y z) = V3 (-y) x z

allOrientations :: Position -> [Position]
allOrientations p = concatMap (\(facing, rot) -> take 4 $ iterate rot facing) faces
  where
    faces =
      [ (p, rotX)
      , (rotZ p, rotY)
      , (rotZ $ rotZ p, rotX)
      , (rotZ . rotZ $ rotZ p, rotY)
      , (rotY p, rotZ)
      , (rotY . rotY $ rotY p, rotZ)
      ]

-- day19a :: String -> Int
day19a = length . matchAll . parse

day19b :: String -> Int
day19b = undefined

positionFromList :: [Int] -> Position
positionFromList [x, y, z] = V3 x y z
positionFromList x = error $ "Tried to make a V3 from " ++ show x

parse :: String -> M.Map Int (S.Set Position)
parse = M.fromList . map parseScanner . splitOn "\n\n"

parseScanner :: String -> (Int, S.Set Position)
parseScanner input = (n, positions)
  where
    n = read . (!! 2) . splitOn " " . head . lines $ input
    positions = S.fromList . map (positionFromList . map read . splitOn ",") . tail . lines $ input

manhattan :: Position -> Position -> Int
manhattan (V3 x y z) (V3 x' y' z') = abs (x - x') + abs (y - y') + abs (z - z')

-- Transforms N positions to N(N-1)/2 distances between the positions, as a sort
-- of fingerprint
fingerprint :: S.Set Position -> S.Set Int
fingerprint positions = S.fromList [manhattan p p' | (p:ps) <- tails $ S.toList positions, p' <- ps]

-- Transforms N positions to N(N-1)/2 distances between the positions, as a sort
-- of fingerprint
vectorFingerprint :: S.Set Position -> S.Set Position
vectorFingerprint positions = S.fromList [p - p' | (p:ps) <- tails $ S.toList positions, p' <- ps]

-- Check if two scanner's sets of beacons overlap - if there are 12*11/2 of the
-- same distances then this is likely.
matchesTwo :: S.Set Position -> S.Set Position -> Bool
matchesTwo s s' = (>= 6 * 11) . S.size $ S.intersection f f'
  where
    f = fingerprint s
    f' = fingerprint s'

-- Given two scanner's sets of beacons, try to orientate the second such that
-- there are at least 12 points which overlap modulo a translation.
matchTwo :: S.Set Position -> S.Set Position -> Maybe (S.Set Position)
matchTwo xs ys =
  case foldl' go Nothing . transpose . map allOrientations $ S.toList ys of
    Just (offset, ys') -> Just $ foldl' (flip S.insert) xs $ map (+ offset) ys'
    Nothing -> Nothing
  where
    go :: Maybe (Position, [Position]) -> [Position] -> Maybe (Position, [Position])
    go (Just x) _ = Just x
    go Nothing flippedYs =
      if snd mostCommonDiff >= 12
        then Just (fst mostCommonDiff, flippedYs)
        else Nothing
      where
        mostCommonDiff =
          maximumBy (comparing snd) . M.toList . M.fromListWith (+) $
          [(x - y, 1) | x <- S.toList xs, y <- flippedYs]

matchAll :: M.Map Int (S.Set Position) -> S.Set Position
matchAll m = go (m M.! 0) (M.delete 0 m)
  where
    go :: S.Set Position -> M.Map Int (S.Set Position) -> S.Set Position
    go ps m =
      if M.null m
        then ps
        else case M.foldlWithKey' (tryMatch ps m) Nothing m of
               Just (newPs, newM) -> go newPs newM
               Nothing -> error "Didn't find any matches"
                 where 
    tryMatch ::
         S.Set Position
      -> M.Map Int (S.Set Position)
      -> Maybe (S.Set Position, M.Map Int (S.Set Position))
      -> Int
      -> S.Set Position
      -> Maybe (S.Set Position, M.Map Int (S.Set Position))
    tryMatch _ _ (Just x) _ _ = Just x
    tryMatch ps m Nothing key potentials =
      case matchTwo ps potentials of
        Just newPs -> Just (newPs, M.delete key m)
        Nothing -> Nothing
